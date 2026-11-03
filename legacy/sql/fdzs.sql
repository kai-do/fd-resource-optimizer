-- LEGACY — quarantined. Not owner-vetted as a whole. Do not build new code on this; copy vetted pieces into the new structure.
select
    [OBJECTID]            as object_id
,   [ChiefDistrict]       as chief_district
,   [Battalion]           as battalion
,   [Station]             as station
,   [Shift]               as shift
,   [FireDemandZone]      as fire_demand_zone
,   [GlobalID]            as global_id
,   [Creator]             as creator
,   [CreationDate]        as creation_date
,   [Editor]              as editor
,   [EditDate]            as edit_date
,   [ResponseAgency]      as response_agency
,   [Shape].STSrid        as srid
,   [Shape].STAsText()    as shape_wkt

from [FIRE].[gisadmin].[FireDemandZones_evw]

order by
    object_id