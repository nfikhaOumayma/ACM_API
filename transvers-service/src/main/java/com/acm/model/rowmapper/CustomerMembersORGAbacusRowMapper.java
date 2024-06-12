/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.CustomerMemberDTO;

/**
 * {@link CustomerMembersORGAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class CustomerMembersORGAbacusRowMapper implements RowMapper<CustomerMemberDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public CustomerMemberDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new CustomerMemberDTO(null, rs.getLong("OrganisationID"), rs.getLong("CustomerID"),
				"Partner");
	}
}
