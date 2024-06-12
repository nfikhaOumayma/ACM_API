/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.stereotype.Service;

import com.acm.constants.common.CommonLoggerMessage;
import com.acm.service.LoadDataAccountGLService;

/**
 * {@link LoadDataAccountGLServiceImpl} class.
 *
 * @author yesser.somai
 * @since 1.0.8
 */
@Service
public class LoadDataAccountGLServiceImpl implements LoadDataAccountGLService {

	/** logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(LoadDataAccountGLServiceImpl.class);
	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoadDataAccountGLService#findAccountGlList(java.lang.Long)
	 */
	@Override
	public List<String> findAccountGlList(Long branchId) {

		try {
			// load query
			String query = environment.getProperty("query.select.GL.Account");
			logger.info("findAccountGlList in given branchId= {}", branchId);

			// init params findAccountGl categoryId = 8 for Expenses Account GL (select * from
			// Category) and given branchId
			SqlParameterSource namedParameters = new MapSqlParameterSource()
					.addValue("categoryId", 8).addValue("branchId", branchId);
			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new RowMapper<String>() {
						@Override
						public String mapRow(ResultSet rs, int rowNum) throws SQLException {

							return rs.getString("number");
						}
					});
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.GL.Account### {}", e.getMessage());
		}
		return new ArrayList<>();
	}

}
