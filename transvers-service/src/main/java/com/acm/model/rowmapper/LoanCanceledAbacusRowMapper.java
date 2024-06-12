/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.LoanDTO;

/**
 * {@link LoanCanceledAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class LoanCanceledAbacusRowMapper implements RowMapper<LoanDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public LoanDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		LoanDTO loanDTO = new LoanDTO();
		loanDTO.setIdLoanExtern(rs.getLong("CULoanID"));
		loanDTO.setProductId(rs.getInt("ProductID"));
		loanDTO.setCustomerId(rs.getLong("CUSTOMERID"));
		return loanDTO;
	}
}
