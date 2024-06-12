/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.UserDefinedFieldGroupDTO;

/**
 * {@link UDFGroupAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class UDFGroupAbacusRowMapper implements RowMapper<UserDefinedFieldGroupDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public UserDefinedFieldGroupDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new UserDefinedFieldGroupDTO(rs.getLong("ABACUS_UDFGROUPEID"), rs.getString("Code"),
				rs.getString("Description"), rs.getLong("LOAN"), rs.getInt("CUSTOMERTYPE"),
				rs.getLong("CUSTOMER"), rs.getString("PRODUCTID"), rs.getBoolean("Active"),
				rs.getBoolean("Mandatory"));
	}
}
