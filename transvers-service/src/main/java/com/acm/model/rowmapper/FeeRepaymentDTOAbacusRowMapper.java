/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

/**
 * {@link FeeRepaymentDTOAbacusRowMapper} class.
 *
 * @author Salmen Fatnassi
 * @since 1.0.8
 */
public class FeeRepaymentDTOAbacusRowMapper implements RowMapper<Long> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public Long mapRow(ResultSet rs, int rowNum) throws SQLException {

		return rs.getLong("ADMIN_FEE");
	}
}
