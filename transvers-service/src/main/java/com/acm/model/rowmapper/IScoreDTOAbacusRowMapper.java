/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.IScoreDTO;

/**
 * {@link IScoreDTOAbacusRowMapper} class.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
public class IScoreDTOAbacusRowMapper implements RowMapper<IScoreDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public IScoreDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		IScoreDTO iScoreDTO = new IScoreDTO();
		iScoreDTO.setiScoreLoan(rs.getString("I_SCORE_LOAN"));
		iScoreDTO.setiScoreCustomer(rs.getString("I_SCORE_CUSTOMER"));
		return iScoreDTO;
	}
}
