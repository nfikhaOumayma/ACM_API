/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.SettingListValuesDTO;

/**
 * {@link SettingListValuesRowMapper} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class SettingListValuesRowMapper implements RowMapper<SettingListValuesDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public SettingListValuesDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new SettingListValuesDTO("Journal", rs.getString("JournalID"),
				rs.getString("Description"), null);
	}
}
