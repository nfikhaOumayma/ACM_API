/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.stereotype.Service;

import com.acm.constants.common.CommonLoggerMessage;
import com.acm.model.rowmapper.BranchChangeRowMapper;
import com.acm.service.BranchChangeService;
import com.acm.utils.dtos.BranchChangeDTO;

/**
 * {@link BranchChangeServiceImpl} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Service
public class BranchChangeServiceImpl implements BranchChangeService {
	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(BranchChangeServiceImpl.class);

	/** The environment. */
	@Autowired
	private Environment environment;

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The Constant LAST_BRANCH_CHANGED_ID. */
	private static final String LAST_BRANCH_CHANGED_ID = "lastBranchChangeIdSynchronized";

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.BranchChangeService#find(java.lang.Long)
	 */
	@Override
	public List<BranchChangeDTO> find(String lastBranchChangeIdSynchronized) {

		try {
			// load query
			String query = environment.getProperty("query.select.get.branchChange");
			logger.info("find branchChange  after id = {}", lastBranchChangeIdSynchronized);
			// init params
			SqlParameterSource namedParameters = new MapSqlParameterSource()
					.addValue(LAST_BRANCH_CHANGED_ID, lastBranchChangeIdSynchronized);
			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new BranchChangeRowMapper());

		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.get.branchChange ### {}", e.getMessage());
		}
		// in case of ana error => returning an empty object of customer with id extern = 0
		return new ArrayList<>();
	}
}
