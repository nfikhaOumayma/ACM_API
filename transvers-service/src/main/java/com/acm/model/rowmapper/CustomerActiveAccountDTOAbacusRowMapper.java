/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.CustomerActiveAccountDTO;

/**
 * {@link CustomerActiveAccountDTOAbacusRowMapper} class.
 *
 * @author MoezMhiri
 * @since 1.8.0
 */
public class CustomerActiveAccountDTOAbacusRowMapper
		implements RowMapper<CustomerActiveAccountDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public CustomerActiveAccountDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		CustomerActiveAccountDTO customerActiveAccountDTO = new CustomerActiveAccountDTO();
		customerActiveAccountDTO.setCustomerId(rs.getLong("CustomerID"));
		customerActiveAccountDTO.setLoanId(rs.getLong("CULoanID"));
		return customerActiveAccountDTO;
	}
}
