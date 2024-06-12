/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.CollaterolDTO;

/**
 * {@link CollateralDTOAbacusRowMapper} class.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
public class CollateralDTOAbacusRowMapper implements RowMapper<CollaterolDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public CollaterolDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new CollaterolDTO(rs.getInt(1), rs.getString(2), rs.getDate(3), rs.getString(4),
				rs.getInt(5), rs.getString(6), rs.getString(7), rs.getString(8), rs.getString(9),
				rs.getString(10), rs.getString(11), rs.getString(12), rs.getString(13),
				rs.getString(14), rs.getString(15), rs.getString(16), rs.getBigDecimal(17),
				rs.getBigDecimal(18), rs.getBigDecimal(19), rs.getBigDecimal(20),
				rs.getBigDecimal(21));

	}
}
