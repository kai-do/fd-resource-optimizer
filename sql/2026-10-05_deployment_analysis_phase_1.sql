use Incident_Reporting

declare @start_year     int      = 2022
declare @end_year       int      = 2025
declare @als_cutover    datetime = '2025-02-16 07:00:00' -- derived from the max date in cad for most EA units. Denotes the switch in naming schemes from EA to E. Assuming select Engines after this point are ALS

;   with cad_incidents__raw as
    (
        select distinct -- duplicate entries in the source db
            ci.incident_number
        ,   ci.cad_incident_key
        ,   ci.incident_date
        ,   ci.incident_started_date_time
        ,   loc.latitude
        ,   loc.longitude
        ,   case
                when ci.cad_initial_incident_type_code = 'HAZ1'   then 'HAZ-1'
                when ci.cad_initial_incident_type_code = 'HAZ2'   then 'HAZ-2'
                when ci.cad_initial_incident_type_code = 'WIRES1' then 'WIRES'
                else ci.cad_initial_incident_type_code
            end                 as cad_initial_incident_type_code
        ,   case
                when ci.cad_incident_type_code = 'HAZ1'   then 'HAZ-1'
                when ci.cad_incident_type_code = 'HAZ2'   then 'HAZ-2'
                when ci.cad_incident_type_code = 'WIRES1' then 'WIRES'
                else ci.cad_incident_type_code
            end                 as cad_incident_type_code
        ,   ci.cad_incident_type_description
        ,   loc.battalion
        ,   loc.station
        ,   loc.fdz
        ,   loc.city_code
        ,   loc.city_name

        from [Incident_Reporting].[dbo].[cad_incident] ci
            inner join dbo.cad_incident_location loc on loc.cad_incident_key = ci.cad_incident_key
    )

,   cad_incidents__date as (select * from cad_incidents__raw where incident_date >= datefromparts(@start_year, 1, 1) and incident_date < dateadd(year, 1, datefromparts(@end_year, 1, 1)))
,   cad_incidents__lat_long as (select * from cad_incidents__date where latitude is not null and longitude is not null)
,   cad_incidents__city as (select * from cad_incidents__lat_long where city_code is not null and city_code not in ('MA', 'SM', 'AU'))
,   cad_incidents as (select * from cad_incidents__city where cad_incident_type_code is not null and cad_incident_type_code not in ('FMSG', 'FTEST'))

,   cad_unit_response as
    (
        select distinct -- duplicate entries in the source db (on trip num)
            cad_unit.callsign
        ,   cad_unit.incident_number
        ,   cad_unit.dispatch_date_time
        ,   cad_unit.enroute_date_time
        ,   cad_unit.on_scene_date_time
        ,   cad_unit.clear_date_time
        ,   case
                when callsign = '501'                           then 'Battalion Chief'
                when callsign = 'SQ17'                          then 'Hazmat Squad'
                when callsign in ('SQ4','SQ7','SQ12','SQ16')    then 'TRT Squad'
                when callsign in ('SQ55', 'SQ52')               then 'TRT Squad'
                when callsign = 'TAC16'                         then 'Med Unit'
                when callsign in ('900', '901')                 then 'Unit 900'
                when callsign in ('E1','E2','E4','E5','E9','E11','E12','E14','E19','E23','E24','E26','E27','E30','E51','E52','E53','E54','E55','E56','E71','E72')
                    and incident_date_time >= @als_cutover      then 'ALS Engine'
                when callsign in ('T1','T19')
                    and incident_date_time >= @als_cutover      then 'ALS Tiller'
                when callsign in ('T1','T4','T8','T19')         then 'BLS Tiller'
                when callsign = 'CARSE3'        then 'Cares Unit'
                when callsign = 'GSAR'          then 'TRT Squad'
                when callsign = 'HELO1'         then 'Other'
                when callsign = 'TR4'           then 'Trench Rescue'

                when callsign like '%CONSP%'    then 'Confined Space Rescue'
                when callsign like '%BATT%'     then 'Battalion Chief'

                when callsign like 'MEDOP%'     then 'MedOps'
                when callsign like 'METRO%'     then 'Ambulance'
                when callsign like 'PUCKT%'     then 'Ambulance'
                when callsign like 'DECON%'     then 'Decon Unit'
                when callsign like 'CARES%'     then 'Cares Unit'
                when callsign like 'ROVER%'     then 'Other'
                when callsign like 'TRAIL%'     then 'Other' --look into adding to SAR trt

                when callsign like 'TMHK%'      then 'ARFF'
                when callsign like 'CLPS%'      then 'Collapse Rescue'
                when callsign like 'DIST%'      then 'District Chief'
                when callsign like 'BOAT%'      then 'Boat'
                when callsign like 'TAMI%'      then 'Other'

                when callsign like 'AIR%'       then 'Air Truck'
                when callsign like 'MED%'       then 'Med Unit'
                when callsign like 'TAC%'       then 'Rescue'
                when callsign like 'RFD%'       then 'Other'

                when callsign like 'SR%'        then 'Rescue'
                when callsign like 'SE%'        then 'ALS Engine'
                when callsign like 'ST%'        then 'ALS Truck'
                when callsign like 'EA%'        then 'ALS Engine'
                when callsign like 'EC%'        then 'Other'
                when callsign like 'AE%'        then 'ALS Engine'
                when callsign like 'AT%'        then 'ALS Truck'
                when callsign like 'UT%'        then 'Other'
                when callsign like 'DC%'        then 'District Chief'
                when callsign like 'TX%'        then 'Other'
                when callsign like 'TR%'        then 'Other'

                when callsign like 'E%'         then 'BLS Engine'
                when callsign like 'T%'         then 'BLS Truck'
                when callsign like 'R%'         then 'Rescue'
                when callsign like 'H%'         then 'Hazmat Unit'

                else 'Unmapped'
            end                 as unit_type

        from [Incident_Reporting].[dbo].[cad_unit_response] cad_unit
    )

,   cad_unit_first_dispatch as
    (
        -- one row per unit per incident: the unit's first dispatch.
        -- arrival is kept as a flag so cancelled/non-arriving units stay in.
        select
            x.*
        ,   row_number() over (
                partition by x.incident_number
                order by
                    case when x.dispatch_date_time is null then 1 else 0 end
                ,   x.dispatch_date_time asc
                ,   x.callsign asc
            )                   as dispatch_seq
        ,   convert(bit, case when x.on_scene_date_time is not null then 1 else 0 end) as unit_arrived_flag

        from
        (
            select
                cur.*
            ,   row_number() over (
                    partition by cur.incident_number, cur.callsign
                    order by
                        case when cur.dispatch_date_time is null then 1 else 0 end
                    ,   cur.dispatch_date_time asc
                    ,   cur.on_scene_date_time asc
                )               as unit_rn

            from cad_unit_response cur
        ) x

        where
            x.unit_rn = 1
    )

,   imagetrend as
    (
        -- one RMS record per incident number: the primary exposure
        select
            iti.fire_incident_number
        ,   iti.fire_exposure
        ,   iti.incident_type_category_code
        ,   iti.incident_type_category
        ,   iti.incident_type_subcategory_code
        ,   iti.incident_type_subcategory
        ,   iti.incident_type_code
        ,   iti.incident_type_description
        ,   row_number() over (
                partition by iti.fire_incident_number
                order by
                    try_convert(int, iti.fire_exposure) asc
                ,   iti.fire_internal_id asc
            )                   as exposure_rn

        from [Incident_Reporting].[dbo].[imagetrend_incident] iti
    )

,   proqa_category as
    (
        -- one row per protocol; proqa_code repeats it once per determinant/suffix
        select distinct
            pc.proqa_case_type
        ,   pc.dispatch_code
        ,   pc.dispatch_category
        ,   pc.dispatch_category_description
        ,   pc.dispatch_code_description


        from dbo.proqa_code pc
    )

select
    cad.incident_number
,   cad.incident_date
,   cad.battalion
,   cad.station
,   cad.fdz
,   cad.city_code

,   pq.proqa_case_type
,   pq.dispatch_code                    as proqa_dispatch_code
,   pq.dispatch_category                as proqa_dispatch_category
,   pcat.dispatch_category_description  as proqa_dispatch_category_description
,   pcat.dispatch_code_description      as proqa_dispatch_code_description
,   pq.dispatch_level                   as proqa_dispatch_level
,   pq.dispatch_sub_category            as proqa_dispatch_sub_category

,   cad.cad_initial_incident_type_code
,   cad.cad_incident_type_code
,   cad.cad_incident_type_description
,   convert(bit, case
        when isnull(cad.cad_initial_incident_type_code, '') <> isnull(cad.cad_incident_type_code, '') then 1
        else 0
    end)                                as cad_type_changed_flag

,   it.incident_type_category_code      as rms_incident_type_category_code
,   it.incident_type_category           as rms_incident_type_category
,   it.incident_type_subcategory_code   as rms_incident_type_subcategory_code
,   it.incident_type_subcategory        as rms_incident_type_subcategory
,   it.incident_type_code               as rms_incident_type_code
,   it.incident_type_description        as rms_incident_type_description

,   unit.dispatch_seq
,   unit.callsign
,   unit.unit_type
,   unit.dispatch_date_time
,   unit.enroute_date_time
,   unit.on_scene_date_time
,   unit.clear_date_time
,   unit.unit_arrived_flag

from cad_incidents cad
    inner join cad_unit_first_dispatch unit on unit.incident_number = cad.incident_number
    left join dbo.cad_incident_proqa pq on pq.cad_incident_key = cad.cad_incident_key
    left join proqa_category pcat on pcat.proqa_case_type = pq.proqa_case_type and pcat.dispatch_code = pq.dispatch_code
    left join imagetrend it on it.fire_incident_number = cad.incident_number and it.exposure_rn = 1

order by
    cad.incident_date
,   cad.incident_number
,   unit.dispatch_seq