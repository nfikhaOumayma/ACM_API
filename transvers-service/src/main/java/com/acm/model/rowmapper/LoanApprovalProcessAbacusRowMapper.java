/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.LoanApprovalProcessDTO;

/**
 * {@link LoanApprovalProcessAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.9
 */
public class LoanApprovalProcessAbacusRowMapper implements RowMapper<LoanApprovalProcessDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public LoanApprovalProcessDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new LoanApprovalProcessDTO(rs.getLong("CULoanProcessID"), rs.getLong("CULoanID"),
				rs.getLong("Completed"), rs.getLong("LoanApprovalGroup"),
				rs.getLong("CustomerType"), rs.getLong("CommunityCULoanID"));
	}
}
