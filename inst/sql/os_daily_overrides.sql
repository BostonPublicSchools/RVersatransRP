SELECT * FROM
(
  SELECT
    *,
    RANK() OVER (PARTITION BY RPRoute, EffectiveDate ORDER BY ChangeDateTime DESC) AS q01
  FROM VRW_v_VTOS_DailyVehicleOverride_History
  ) a1
  WHERE a1.q01 = 1 AND a1.ReasonForChange = '';
