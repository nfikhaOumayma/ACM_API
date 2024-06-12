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
import com.acm.model.rowmapper.GuarantorDTOAbacusRowMapper;
import com.acm.service.GuarantorAbacusService;
import com.acm.utils.dtos.GuarantorDTO;

/**
 * {@link GuarantorAbacusServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@Service
public class GuarantorAbacusServiceImpl implements GuarantorAbacusService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(GuarantorAbacusServiceImpl.class);

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.GuarantorAbacusService#find(java.lang.Long)
	 */
	@Override
	public List<GuarantorDTO> find(Long idLoan) {

		try {
			// load query
			String query = environment.getProperty("query.select.loan.guarantor");
			logger.info("findGuarantor : idLoan= {}", idLoan);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("id_loan", idLoan);
			// execute query
			List<GuarantorDTO> guarantorDTOs = namedParameterJdbcTemplate.query(query,
					namedParameters, new GuarantorDTOAbacusRowMapper());
			logger.info(" find GUARANTOR data from ABACUS DB :: DONE");
			// returning result
			return guarantorDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY ### {}", e.getMessage());
		}
		return new ArrayList<>();
	}
}
