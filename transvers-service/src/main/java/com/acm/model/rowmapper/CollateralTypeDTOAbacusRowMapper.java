/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.CollateralTypeDTO;

/**
 * {@link CollateralTypeDTOAbacusRowMapper} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class CollateralTypeDTOAbacusRowMapper implements RowMapper<CollateralTypeDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public CollateralTypeDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new CollateralTypeDTO(rs.getLong(1), rs.getString(2), rs.getString(3), rs.getLong(4),
				rs.getBigDecimal(5), rs.getBoolean(6), rs.getString(7), rs.getBoolean(8),
				rs.getBoolean(9));
	}
}
