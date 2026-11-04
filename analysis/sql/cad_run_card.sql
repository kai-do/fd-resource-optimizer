with stations as (
	select
		location_id = rc.[RunCardKey]
	,	location_type = [GeographicLevel]
	,	location = [GeographicValue]
	,	location_name = [LocationName]
	,	city = [City]
	,	agency = [RunCard_AgencyId]
	,	responding_value = [RunCardOrderValue]
	,	response_sequence = row_number() over (partition by rc.RunCardKey order by RunCardOrderValueSequence#)
	,	responding_station_id = convert(int, right([RunCardOrderValue], len([RunCardOrderValue]) - CHARINDEX('/', [RunCardOrderValue])))
	
	from [PONE_RDW].[ReportingDW].[dbo].[RSVM_RunCard] rc
		inner join [PONE_RDW].[ReportingDW].[dbo].[RSVM_RunCard_Order] rco on rco.RunCardKey = rc.RunCardKey
 
	where 
		rc.IsActive = 'yes'
	and rco.IsActive = 'yes'
	and [OrderType] = 'Station'
	and [RunCard_AgencyId] in ('CF','MF','AF')
	and len([RunCardOrderValue]) <= 5

)

,	battalions as (
	select
		location_id = rc.[RunCardKey]
	,	location_type = [GeographicLevel]
	,	location = [GeographicValue]
	,	location_name = [LocationName]
	,	city = [City]
	,	agency = [RunCard_AgencyId]
	,	responding_value = [RunCardOrderValue]
	,	response_sequence = row_number() over (partition by rc.RunCardKey order by RunCardOrderValueSequence#)
	,	responding_battalion_id = convert(int, right([RunCardOrderValue], len([RunCardOrderValue]) - CHARINDEX('/', [RunCardOrderValue])))
	
	from [PONE_RDW].[ReportingDW].[dbo].[RSVM_RunCard] rc
		inner join [PONE_RDW].[ReportingDW].[dbo].[RSVM_RunCard_Order] rco on rco.RunCardKey = rc.RunCardKey
 
	where 
		rc.IsActive = 'yes'
	and rco.IsActive = 'yes'
	and [OrderType] = 'Station'
	and [RunCard_AgencyId] in ('CF','MF','AF')
	and len([RunCardOrderValue]) = 6

)

select
	sta.*
,	batt.responding_battalion_id

--into dbo.cad_run_card

from stations sta
left join battalions batt on batt.location_id = sta.location_id and batt.response_sequence = sta.response_sequence

OPTION (USE HINT('FORCE_LEGACY_CARDINALITY_ESTIMATION'));