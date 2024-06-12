/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.SettingMotifRejetsDTO;

/**
 * {@link SettingMotifRejetsDTORowMapper} class.
 *
 * @author ManelLamloum
 * @since 1.1.5
 */
public class SettingMotifRejetsDTORowMapper implements RowMapper<SettingMotifRejetsDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public SettingMotifRejetsDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		SettingMotifRejetsDTO settingMotifRejetsDTO = new SettingMotifRejetsDTO();
		settingMotifRejetsDTO.setCode(rs.getString("Code"));

		settingMotifRejetsDTO.setCodeExternal(rs.getInt("CancelReasonID"));

		settingMotifRejetsDTO.setDescription(rs.getString("Description"));

		return settingMotifRejetsDTO;
	}
}
