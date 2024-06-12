/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.LoanDistrictCodeDTO;

/**
 * {@link LoanDistrictCodeAbacusRowMapper} class.
 *
 * @author MoezMhiri
 * @since 1.0.7
 */
public class LoanDistrictCodeAbacusRowMapper implements RowMapper<LoanDistrictCodeDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public LoanDistrictCodeDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new LoanDistrictCodeDTO(rs.getLong("CULoanDistictCodeID"), rs.getString("Code"),
				rs.getString("Description"), rs.getInt("DefaultItem"), rs.getBoolean("Active"));
	}
}
