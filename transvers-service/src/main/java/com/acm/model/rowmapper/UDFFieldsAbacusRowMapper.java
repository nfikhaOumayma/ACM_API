/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;

/**
 * {@link UDFFieldsAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class UDFFieldsAbacusRowMapper implements RowMapper<UserDefinedFieldsDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public UserDefinedFieldsDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new UserDefinedFieldsDTO(
				new UserDefinedFieldGroupDTO(rs.getLong("ABACUS_USFGROUPEID")),
				rs.getLong("ABACUS_UDFFIELDID"), rs.getLong("PARENTUDFFIELDID"),
				rs.getString("PARENTUDFFIELDVALUE"), rs.getString("Mask"),
				rs.getString("FIELDNAME"), rs.getString("Description"), rs.getBoolean("Mandatory"),
				rs.getInt("FIELD_TYPE"), rs.getLong("UserDefinedFieldListID"),
				rs.getBoolean("Active"), rs.getBoolean("UniqueField"), rs.getInt("Ordre"));
	}
}
