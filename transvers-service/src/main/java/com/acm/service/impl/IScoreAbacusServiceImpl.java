/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
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
import com.acm.model.rowmapper.IScoreDTOAbacusRowMapper;
import com.acm.service.IScoreAbacusService;
import com.acm.utils.dtos.IScoreDTO;

/**
 * {@link IScoreAbacusServiceImpl} class.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
@Service
public class IScoreAbacusServiceImpl implements IScoreAbacusService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(IScoreAbacusServiceImpl.class);

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IScoreAbacusService#findIScore()
	 */
	@Override
	public List<IScoreDTO> findIScore(String startDate, String endDate) {

		try {
			// load query
			String query = environment.getProperty("query.select.i-score");
			// execute query
			Date formatedStartDate = (new SimpleDateFormat("yyyy-MM-dd")).parse(startDate);
			Date formatedEndDate = (new SimpleDateFormat("yyyy-MM-dd")).parse(endDate);

			SqlParameterSource namedParameters = new MapSqlParameterSource()
					.addValue("dateStart", formatedStartDate).addValue("dateEnd", formatedEndDate);
			List<IScoreDTO> iScoreDTOs = namedParameterJdbcTemplate.query(query, namedParameters,
					new IScoreDTOAbacusRowMapper());
			logger.info("Find I-score data from ABACUS DB :: {} records loaded ",
					iScoreDTOs.size());
			// returning result
			return iScoreDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.i-score ### {}", e.getMessage());
		}
		return new ArrayList<>();
	}
}
