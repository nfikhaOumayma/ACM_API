/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.UserDTO;

/**
 * {@link UserDTOAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
public class UserDTOAbacusRowMapper implements RowMapper<UserDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public UserDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new UserDTO(rs.getString("UserName"), rs.getLong("CUAccountPortfolioID"),
				rs.getString("SupervisorUserID"), rs.getLong("UserID"), rs.getLong("UserProfileID"),
				rs.getBoolean("active"), rs.getInt("BranchID"), rs.getString("BRANCHE_NAME"),
				rs.getString("BRANCHE_DESCRIPTION"));
	}
}
