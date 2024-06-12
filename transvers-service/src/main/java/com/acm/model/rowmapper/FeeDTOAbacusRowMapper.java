/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

/**
 * The Class FeeDTOAbacusRowMapper.
 */
public class FeeDTOAbacusRowMapper implements RowMapper<Long> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public Long mapRow(ResultSet rs, int rowNum) throws SQLException {

		return rs.getLong("FEE");
	}

}
