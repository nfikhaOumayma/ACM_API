/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.PortfolioDTO;

/**
 * The Class PortfolioDTOAbacusRowMapper.
 * 
 * @author Salmen Fatnassi
 * @since 1.1.2
 */
public class PortfolioDTOAbacusRowMapper implements RowMapper<PortfolioDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public PortfolioDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new PortfolioDTO(rs.getLong("CUAccountPortfolioID"), rs.getString("Description"),
				rs.getBoolean("Active"));
	}
}
