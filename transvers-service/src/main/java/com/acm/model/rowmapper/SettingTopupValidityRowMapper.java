/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.SettingTopupValidityDTO;

/**
 * The Class SettingTopupValidityRowMapper.
 */
public class SettingTopupValidityRowMapper implements RowMapper<SettingTopupValidityDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public SettingTopupValidityDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		SettingTopupValidityDTO settingTopupValidityDTO = new SettingTopupValidityDTO();
		settingTopupValidityDTO
				.setMaxContinuousLateDaysOrMaxSeparateLateDaysValidity(rs.getInt("br1"));
		settingTopupValidityDTO.setMinLoanPaymentPercentageValidity(rs.getBoolean("br0"));
		settingTopupValidityDTO.setMinPreviouslyIssuedLoansNumberValidity(rs.getBoolean("br4"));
		return settingTopupValidityDTO;
	}
}
