/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.ProductDTO;

/**
 * {@link ProductDTOAnalyticsRowMapper} class.
 *
 * @author YesserSomai
 * @since 0.11.0
 */
public class ProductDTOAnalyticsRowMapper implements RowMapper<ProductDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public ProductDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		ProductDTO productDTO = new ProductDTO();
		productDTO.setMaximumBalance(rs.getBigDecimal("MAXIMUMBALANCE"));
		productDTO.setMinimumTerm(rs.getInt("MINIMUMTERM"));
		productDTO.setMaximumTerm(rs.getInt("MAXIMUMTERM"));
		return productDTO;
	}
}
