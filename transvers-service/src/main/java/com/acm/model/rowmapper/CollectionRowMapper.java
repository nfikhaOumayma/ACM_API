/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.AcmCollectionDTO;

/**
 * {@link CollectionRowMapper} class.
 *
 * @author idridi
 * @since 1.1.9
 */
public class CollectionRowMapper implements RowMapper<AcmCollectionDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public AcmCollectionDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		AcmCollectionDTO acmCollectionDTO = new AcmCollectionDTO();
		acmCollectionDTO.setTypeCustomer(rs.getString("CustomerType"));
		acmCollectionDTO.setCustomerIdExtern(rs.getLong("CustomerIdExtern"));
		acmCollectionDTO.setAccountNumber(rs.getString("AccountNumber"));
		acmCollectionDTO.setProductDescription(rs.getString("ProductName"));
		acmCollectionDTO.setCustomerName(rs.getString("CustomerName"));
		acmCollectionDTO.setBranchDescription(rs.getString("BranchName"));
		acmCollectionDTO.setAmount(rs.getBigDecimal("LoanAmount"));
		acmCollectionDTO.setLoanOfficer(rs.getString("loanOfficer"));
		acmCollectionDTO.setFirstUnpaidInstallment(rs.getDate("FirstUnPaidInstallement"));
		acmCollectionDTO.setUnpaidAmount(rs.getBigDecimal("UnpaidAmount"));
		acmCollectionDTO.setLateDays(rs.getInt("LateDays"));
		acmCollectionDTO.setNumberOfUnpaidInstallment(rs.getInt("NBUnpaidInstallement"));
		acmCollectionDTO.setProductId(rs.getLong("Product_ID"));
		acmCollectionDTO.setBranchId(rs.getLong("BranchID"));
		acmCollectionDTO.setCurrencyDecimalPlaces(rs.getInt("Currency_Decimal_Places"));
		acmCollectionDTO.setCurrencySymbol(rs.getString("Currency_Symbol"));
		acmCollectionDTO.setIdLoanExtern(rs.getLong("CuLoanId"));
		return acmCollectionDTO;
	}

}
