-- LEGACY — quarantined. Not owner-vetted as a whole. Do not build new code on this; copy vetted pieces into the new structure.
select
    [OBJECTID]                   as object_id
,   [ADDRESS]                    as address
,   [TYPE]                       as type
,   [PHONE]                      as phone
,   [URL]                        as url
,   ltrim(rtrim([ABBREV]))       as abbrev
,   replace([NAME], '  ', ' ')   as name
,   [DEPT]                       as dept
,   [EDITOR]                     as editor
,   [EDITDATE]                   as edit_date
,   [SHAPE].STSrid               as srid
,   [SHAPE].STX                  as x
,   [SHAPE].STY                  as y
,   [SHAPE].STAsText()           as shape_wkt

from [FIRE].[gisadmin].[FIRESTATIONS_EVW]

order by
    object_id