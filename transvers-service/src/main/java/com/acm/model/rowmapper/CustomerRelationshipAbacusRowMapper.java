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
 * {@link CustomerRelationshipAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.9
 */
public class CustomerRelationshipAbacusRowMapper implements RowMapper<CustomerMemberDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public CustomerMemberDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new CustomerMemberDTO(rs.getLong("childCustomerID"), rs.getLong("CustomerID"),
				rs.getString("name"), rs.getLong("RelationshipID"));
	}
}
