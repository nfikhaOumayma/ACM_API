/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.CustomerDTO;

/**
 * {@link CustomerLoanDTOAbacusRowMapper} class.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
public class CustomerLoanDTOAbacusRowMapper implements RowMapper<CustomerDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public CustomerDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		CustomerDTO customerDTO = new CustomerDTO();
		customerDTO.setCustomerIdExtern(rs.getLong("CUSTOMERID"));
		customerDTO.setCustomerNumber(rs.getString("CUSTOMER_NUMBER"));
		customerDTO.setCustomerName(rs.getString("CUSTOMER_NAME"));
		customerDTO.setCustomerOpenDate(rs.getDate("CUSTOMER_OPENDATE"));
		customerDTO.setDateOfBirth(rs.getDate("DATEOFBIRTH"));
		customerDTO.setAccountPortfolioID(rs.getLong("ACCOUNTPORTFOLIOID"));
		customerDTO.setAccountPortfolioCode(rs.getString("PORTFOLIO_CODE"));
		customerDTO.setAccountPortfolioDescription(rs.getString("PORTFOLIO_DESCRIPTION"));
		customerDTO.setCustomerAddress(rs.getString("CUSTOMER_ADDRESS"));
		customerDTO.setBranchId(rs.getInt("BRANCHID"));
		customerDTO.setBranchesName(rs.getString("BRANCHE_NAME"));
		customerDTO.setBranchesDescription(rs.getString("BRANCHE_DESCRIPTION"));
		customerDTO.setArrearDay(rs.getLong("ARREARDAY"));
		customerDTO.setArrearSchedule(rs.getLong("ARREARSCHEDULE"));
		customerDTO.setTelephone1(rs.getString("Telephone1"));
		customerDTO.setTelephone2(rs.getString("Telephone2"));
		customerDTO.setTelephone3(rs.getString("Telephone3"));
		customerDTO.setAltName(rs.getString("AltName"));
		customerDTO.setCorrespondanceName(null);
		customerDTO.setPersonIdExtern(rs.getLong("PersonID"));
		customerDTO.setEmail(rs.getString("EMail"));
		customerDTO.setGender(rs.getString("GENDER"));
		return customerDTO;
	}
}
