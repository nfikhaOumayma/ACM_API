/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.LoanDetailsInfoResponseDTO;

/**
 * The Class LoanDetailsInformationsRowMapper.
 */
public class LoanDetailsInformationsRowMapper implements RowMapper<LoanDetailsInfoResponseDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public LoanDetailsInfoResponseDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		LoanDetailsInfoResponseDTO loanDetailsInfoResponseDTO = new LoanDetailsInfoResponseDTO();
		loanDetailsInfoResponseDTO.setLoanId(rs.getLong("Loan ID"));
		loanDetailsInfoResponseDTO.setRemainingAmount(rs.getDouble("Remaining Amount"));
		loanDetailsInfoResponseDTO.setFirstRepaymentDate(rs.getString("First Repayment Date"));
		loanDetailsInfoResponseDTO.setIrr(rs.getDouble("IRR"));
		loanDetailsInfoResponseDTO.setApr(rs.getDouble("APR"));
		loanDetailsInfoResponseDTO.setTotalFees(rs.getDouble("Total Fees"));
		loanDetailsInfoResponseDTO.setBalance(rs.getDouble("Balance"));
		loanDetailsInfoResponseDTO.setStatus(rs.getString("Status"));
		loanDetailsInfoResponseDTO.setPrincipal(rs.getDouble("Principal"));
		loanDetailsInfoResponseDTO.setPrincipalPaid(rs.getDouble("Principal Paid"));
		loanDetailsInfoResponseDTO.setProfit(rs.getDouble("Profit"));
		loanDetailsInfoResponseDTO.setProfitPaid(rs.getDouble("Profit Paid"));
		loanDetailsInfoResponseDTO.setTotalPaid(rs.getDouble("Total Paid"));
		loanDetailsInfoResponseDTO.setLoanReason(rs.getString("Loan Reason"));

		return loanDetailsInfoResponseDTO;
	}

}
