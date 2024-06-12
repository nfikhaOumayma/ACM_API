/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

/**
 * {@link MaxActiveAccountByCustomerAbacusRowMapper} class.
 * 
 * @author Ines Dridi
 * @since 1.0.1
 */
public class MaxActiveAccountByCustomerAbacusRowMapper implements RowMapper<Long> {

	@Override
	public Long mapRow(ResultSet rs, int rowNum) throws SQLException {

		return rs.getLong("MAX_ACTIVE_ACCOUNT");
	}

}
