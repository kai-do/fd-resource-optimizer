with nfirs as
(
    select
        'NFIRS'                                         as source_system
    ,   b.Basic_Incident_Number                         as incident_number
    ,   b.Incident_ID_Internal                          as source_incident_id
    ,   a.Apparatus_ID_Internal                         as source_unit_response_id
    ,   cast(b.Basic_Incident_Date_Time as date)        as incident_date
    ,   b.Basic_Incident_Alarm_Time                     as incident_alarm_dt
    ,   b.Basic_Primary_Station_Name                    as primary_station
    ,   b.Basic_Shift_Or_Platoon                        as shift
    ,   b.Basic_Incident_Status                         as incident_status
    ,   f.Basic_Incident_Validity_Score                 as validity_score

    ,   b.Basic_Incident_Type_Code                      as incident_type_code
    ,   b.Basic_Incident_Type                           as incident_type_desc
    ,   left(b.Basic_Incident_Type_Code, 1)             as incident_category_code
    ,   b.Basic_Incident_Type_Category                  as incident_category
    ,   left(b.Basic_Incident_Type_Code, 2)             as incident_subcategory_code
    ,   b.Basic_Incident_Type_Subcategory               as incident_subcategory
    ,   cast(null as nvarchar(255))                     as dispatch_determinate_code
    ,   cast(null as nvarchar(506))                     as cad_incident_type

    ,   b.Basic_Incident_Full_Address                   as full_address
    ,   b.Basic_Incident_Street_Number                  as street_number
    ,   b.Basic_Incident_Street_Prefix                  as street_prefix
    ,   b.Basic_Incident_Street_Name                    as street_name
    ,   b.Basic_Incident_Street_Type                    as street_type
    ,   b.Basic_Incident_Street_Suffix                  as street_suffix
    ,   b.Basic_Incident_Apartment_Number               as apartment_number
    ,   b.Basic_Incident_City_Name                      as city
    ,   b.Basic_Incident_County_Name                    as county
    ,   b.Basic_Incident_State                          as state
    ,   b.Basic_Incident_Postal_Code                    as postal_code
    ,   try_cast(b.Basic_Incident_Latitude as decimal(18, 8))   as latitude
    ,   try_cast(b.Basic_Incident_Longitude as decimal(18, 8))  as longitude
    ,   b.Basic_Incident_Geocoded_Latitude              as geocoded_latitude
    ,   b.Basic_Incident_Geocoded_Longitude             as geocoded_longitude

    ,   a.Apparatus_Resource_ID                         as unit_id
    ,   cast(null as nvarchar(255))                     as unit_neris_id
    ,   a.Apparatus_Resource_Vehicle_Call_Sign          as unit_call_sign
    ,   a.Apparatus_Resource_Type_Code                  as unit_type_code
    ,   a.Apparatus_Resource_Type                       as unit_type_desc
    ,   a.Apparatus_Resource_Response_Mode_To_Scene     as response_mode
    ,   a.Apparatus_Resource_Number_Of_People           as personnel_count
    ,   a.Apparatus_Resource_Dispatch_Date_Time         as dispatch_dt
    ,   a.Appratus_Resource_Enroute_Date_Time           as enroute_dt
    ,   a.Apparatus_Resource_Arrival_Date_Time          as on_scene_dt
    ,   a.Apparatus_Resource_Clear_Date_Time            as clear_dt
    ,   a.Apparatus_Resource_In_Service_Date_Time       as in_service_dt

    from [DCP-FIRESQL1PRD].CF_DataMart.DwFire.Dim_ApparatusResources a
        inner join [DCP-FIRESQL1PRD].CF_DataMart.DwFire.Dim_Basic b on b.Incident_ID_Internal = a.Incident_ID_Internal
        left join [DCP-FIRESQL1PRD].CF_DataMart.DwFire.Fact_Fire f on f.Incident_ID_Internal = b.Incident_ID_Internal

    where
        b.Basic_Incident_Date_Time >= '2018-01-01'
)

,   neris_geocode as
(
    -- Pass-through query: excludes the geography (CLR) columns that block distributed joins
    select
        Incident_ID_Internal
    ,   Latitude
    ,   Longitude
    ,   Geocoded_Latitude
    ,   Geocoded_Longitude
    ,   DeletedStatus
    from OPENQUERY([DCP-FIRESQL1PRD], '
        SELECT
            Incident_ID_Internal
        ,   Latitude
        ,   Longitude
        ,   Geocoded_Latitude
        ,   Geocoded_Longitude
        ,   DeletedStatus
        FROM CF_DataMart.DwNerisFire.Geocode_IncidentLocation
    ')
)

,   neris as
(
    select
        'NERIS'                                         as source_system
    ,   i.IncidentNumber                                as incident_number
    ,   i.Incident_ID_Internal                          as source_incident_id
    ,   u.IncidentUnitResponse_ID_Internal              as source_unit_response_id
    ,   i.IncidentDate                                  as incident_date
    ,   e.Incident_AlarmDateTime                        as incident_alarm_dt
    ,   i.Station_Identifier_Display                    as primary_station
    ,   i.Shift                                         as shift
    ,   i.Status_Display                                as incident_status
    ,   i.Validity                                      as validity_score

    ,   i.IncidentTypePrimary_Value                     as incident_type_code
    ,   i.IncidentTypePrimary_Display                   as incident_type_desc
    ,   i.IncidentTypePrimary_Category1                 as incident_category_code
    ,   i.IncidentTypePrimary_Category1                 as incident_category
    ,   i.IncidentTypePrimary_Category2                 as incident_subcategory_code
    ,   i.IncidentTypePrimary_Category2                 as incident_subcategory
    ,   cast(null as nvarchar(255))                     as dispatch_determinate_code
    ,   cast(null as nvarchar(506))                     as cad_incident_type

    ,   il.IncidentLocation_AddressStreet_Display       as full_address
    ,   il.StreetNumber                                 as street_number
    ,   il.StreetPrefixDirection                        as street_prefix
    ,   il.StreetName                                   as street_name
    ,   il.IncidentLocationStreetPostFix_Display        as street_type
    ,   il.StreetPostfixDirection                       as street_suffix
    ,   il.UnitValue                                    as apartment_number
    ,   il.CityName                                     as city
    ,   il.CountyName                                   as county
    ,   il.StateName                                    as state
    ,   il.PostalCode                                   as postal_code
    ,   cast(g.Latitude as decimal(18, 8))              as latitude
    ,   cast(g.Longitude as decimal(18, 8))             as longitude
    ,   cast(g.Geocoded_Latitude as decimal(18, 8))     as geocoded_latitude
    ,   cast(g.Geocoded_Longitude as decimal(18, 8))    as geocoded_longitude

    ,   u.AgencyNerisUnit_UnitName                      as unit_id
    ,   u.UnitNerisID                                   as unit_neris_id
    ,   u.AgencyNerisUnit_UnitName                      as unit_call_sign
    ,   nu.NerisApparatusType_Value                     as unit_type_code
    ,   nu.NerisApparatusType_Display                   as unit_type_desc
    ,   u.IncidentUnitResponseResponseMode_Display      as response_mode
    ,   u.PersonnelCount                                as personnel_count
    ,   u.UnitDispatchTime                              as dispatch_dt
    ,   u.UnitEnrouteToSceneTime                        as enroute_dt
    ,   u.UnitOnSceneTime                                as on_scene_dt
    ,   u.UnitClearTime                                 as clear_dt
    ,   u.UnitInServiceDateTime                         as in_service_dt

    from [DCP-FIRESQL1PRD].CF_DataMart.DwNerisFire.Dim_IncidentUnitResponses u
        inner join [DCP-FIRESQL1PRD].CF_DataMart.DwNerisFire.Dim_Incident i on i.Incident_ID_Internal = u.Incident_ID_Internal
        left join [DCP-FIRESQL1PRD].CF_DataMart.DwNerisFire.Dim_EarliestReportedDateTime e on e.Incident_ID_Internal = i.Incident_ID_Internal
            and e.DeletedStatus = 0
        left join [DCP-FIRESQL1PRD].CF_DataMart.DwNerisFire.Dim_IncidentLocation il on il.Incident_ID_Internal = i.Incident_ID_Internal
            and il.DeletedStatus = 0
        left join neris_geocode g on g.Incident_ID_Internal = i.Incident_ID_Internal
            and g.DeletedStatus = 0
        left join [DCP-FIRESQL1PRD].CF_DataMart.DwNerisResource.Dim_Agency_ManyAgencyNerisUnits nu on nu.Unit_ID_Internal = u.UnitIDReported_ID

    where
        u.DeletedStatus = 0
    and i.DeletedStatus = 0
    and i.Status_Display is not null
)

,   cad as (

    select
	    ci.incident_number
    ,   ci.incident_agency_id
    ,	ci.cad_incident_type_code
    ,	ci.cad_incident_type_description
    ,	pqa.proqa_case_type
    ,	pqa.dispatch_code
    ,	pqa.dispatch_category
    ,   p.dispatch_category_description
    ,	pqa.dispatch_level
    ,   p.dispatch_code_description
    ,	pqa.dispatch_sub_category
    ,   cil.[battalion]
    ,   cil.[station]
    ,   cil.[fdz]
    ,   cil.[city_code]
    ,   cil.[city_name]

    from [Incident_Reporting].[dbo].[cad_incident] ci
      inner join [Incident_Reporting].[dbo].[cad_incident_proqa] pqa on pqa.cad_incident_key = ci.cad_incident_key
      left join [Incident_Reporting].[dbo].[proqa_code] p on p.[dispatch_code] = pqa.[dispatch_code]
      left join [Incident_Reporting].[dbo].[cad_incident_location] cil on cil.cad_incident_key = ci.cad_incident_key

)

,   unioned as
(
    select * from nfirs

    union all

    select * from neris
)

,   unit_responses as (
select
    x.source_system
,   x.incident_number
,   cad.incident_agency_id
,   x.source_incident_id
,   x.source_unit_response_id
,   x.incident_date
,   x.incident_alarm_dt
,   x.primary_station
,   x.shift
,   x.incident_status
,   x.validity_score
,	cad.cad_incident_type_code
,	cad.cad_incident_type_description
,	cad.proqa_case_type
,	cad.dispatch_category
,   cad.dispatch_category_description
,	cad.dispatch_level
,	cad.dispatch_sub_category
,	cad.dispatch_code
,   cad.dispatch_code_description
,   x.incident_type_code
,   x.incident_type_desc
,   x.incident_category_code
,   x.incident_category
,   x.incident_subcategory_code
,   x.incident_subcategory
,   x.dispatch_determinate_code
,   x.cad_incident_type
,   cad.[battalion]
,   cad.[station]
,   cad.[fdz]
,   cad.[city_code]
,   cad.[city_name]
,   x.full_address
,   x.street_number
,   x.street_prefix
,   x.street_name
,   x.street_type
,   x.street_suffix
,   x.apartment_number
,   x.city
,   x.county
,   x.state
,   x.postal_code
,   x.latitude
,   x.longitude
,   x.geocoded_latitude
,   x.geocoded_longitude
,   x.unit_id
,   x.unit_neris_id
,   x.unit_call_sign
,   x.unit_type_code
,   x.unit_type_desc
,   x.response_mode
,   x.personnel_count
,   x.dispatch_dt
,   x.enroute_dt
,   x.on_scene_dt
,   x.clear_dt
,   x.in_service_dt
,   datediff(second, x.dispatch_dt, x.enroute_dt)   as dispatch_to_enroute_sec
,   datediff(second, x.enroute_dt, x.on_scene_dt)   as enroute_to_on_scene_sec
,   datediff(second, x.dispatch_dt, x.on_scene_dt)  as dispatch_to_on_scene_sec


from unioned x
    left join cad on cad.incident_number = x.incident_number

)

select distinct
    source_system
,   incident_agency_id
,   incident_number
,   source_incident_id

,   [battalion]
,   [station]
,   [fdz]
,   [city_code]
,   [city_name]

,   latitude
,   longitude
,   incident_date
,   incident_alarm_dt
,   shift

    -- CAD
,   cad_incident_type_code
,   cad_incident_type_description

    -- Dispatch (ProQA)
,   proqa_case_type                     as dispatch_case_type_description
,   dispatch_category                   as dispatch_category_code
,   dispatch_category_description
,   dispatch_level                      as dispatch_level_code
,   dispatch_sub_category               as dispatch_sub_category_code
,   dispatch_code
,   dispatch_code_description

    -- Incident (RMS)
,   incident_category_code
,   incident_category                   as incident_category_description
,   incident_subcategory_code
,   incident_subcategory                as incident_subcategory_description
,   incident_type_code
,   incident_type_desc                  as incident_type_description

from unit_responses

where 
    incident_status in ('Ready for QA', 'Finalized', 'Successfully Exported')
and validity_score = 100
and on_scene_dt is not null
and latitude is not null
and longitude is not null