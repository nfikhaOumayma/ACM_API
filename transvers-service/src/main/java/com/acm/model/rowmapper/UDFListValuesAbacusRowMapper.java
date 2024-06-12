/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.enums.SettingUDFTable;

/**
 * {@link UDFListValuesAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class UDFListValuesAbacusRowMapper implements RowMapper<UserDefinedFieldListValuesDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public UserDefinedFieldListValuesDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new UserDefinedFieldListValuesDTO(
				SettingUDFTable.USER_DEFINED_FIELD_LIST_VALUES.tableName(),
				rs.getLong("UserDefinedFieldListValueID"), rs.getLong("UserDefinedFieldListID"),
				rs.getInt("Score"), rs.getString("Name"), rs.getString("Description"),
				rs.getBoolean("Active"));
	}
}
