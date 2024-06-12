/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.RelationshipDTO;

/**
 * {@link RelationshipAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class RelationshipAbacusRowMapper implements RowMapper<RelationshipDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public RelationshipDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new RelationshipDTO(rs.getLong("RelationshipID"), rs.getString("Name"),
				rs.getString("InverseName"), rs.getInt("Directional"), rs.getBoolean("Active"));
	}
}
