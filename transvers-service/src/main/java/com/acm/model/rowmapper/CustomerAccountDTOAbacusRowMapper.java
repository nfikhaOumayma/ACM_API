/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.CustomerAccountDTO;

/**
 * {@link CustomerAccountDTOAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 0.4.0
 */
public class CustomerAccountDTOAbacusRowMapper implements RowMapper<CustomerAccountDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public CustomerAccountDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		CustomerAccountDTO customerAccountDTO = new CustomerAccountDTO();
		customerAccountDTO.setCustomerId(rs.getLong("CustomerID"));
		customerAccountDTO.setLoanId(rs.getLong("CULoanID"));
		customerAccountDTO.setCuAccountId(rs.getLong("CUAccountID"));
		customerAccountDTO.setAccount(rs.getString("Accouunt"));
		customerAccountDTO.setIssueAmount(rs.getBigDecimal("IssueAmount"));
		customerAccountDTO.setIssueDate(rs.getDate("IssueDate"));
		customerAccountDTO.setPortfolioId(rs.getString("PortfolioID"));
		customerAccountDTO.setStatutId(rs.getInt("Status"));
		customerAccountDTO.setAccountRating(rs.getString("AccountRating"));
		customerAccountDTO.setBalance(rs.getBigDecimal("Balance"));
		customerAccountDTO.setCanTopup(rs.getBoolean("CanTopup"));
		customerAccountDTO.setProductIdAbacus(rs.getLong("ProductID"));
		customerAccountDTO.setCurrencyCode(rs.getString("currencyCode"));
		return customerAccountDTO;
	}
}
