/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.RoleAbacusDTO;

/**
 * {@link RoleAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class RoleAbacusRowMapper implements RowMapper<RoleAbacusDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public RoleAbacusDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new RoleAbacusDTO(rs.getLong("CURoleID"), rs.getString("code"),
				rs.getString("Description"), rs.getInt("DefaultItem"), rs.getBoolean("Active"),
				rs.getBoolean("UniqueRestriction"), rs.getBoolean("IsGroupHead"));
	}
}
