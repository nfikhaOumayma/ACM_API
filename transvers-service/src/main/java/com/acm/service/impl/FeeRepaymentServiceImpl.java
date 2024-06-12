/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.IOException;
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

import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.model.rowmapper.FeeRepaymentDTOAbacusRowMapper;
import com.acm.service.FeeRepaymentService;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link FeeRepaymentServiceImpl} class.
 *
 * @author Salmen Fatnassi
 * @since 1.0.8
 */
@Service
public class FeeRepaymentServiceImpl implements FeeRepaymentService {
	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(FeeRepaymentServiceImpl.class);

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.FeeRepaymentService#findFeeReoayment(java.lang.Long)
	 */
	@Override
	public Long findFeeRepayment(Long idAccount) {

		try {
			// load query
			String query = environment.getProperty("query.select.fee.repayment.CUTransaction");
			logger.info("findFeeRepayment : idAccount= {}", idAccount);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("id_account", idAccount);

			// execute query
			List<Long> feeRepayments = namedParameterJdbcTemplate.query(query, namedParameters,
					new FeeRepaymentDTOAbacusRowMapper());
			if (!ACMValidationUtils.isNullOrEmpty(feeRepayments)) {
				return feeRepayments.get(0);
			}
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return 0L;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.FeeRepaymentService#findCuFeeId(java.lang.Long)
	 */
	@Override
	public Long findCuFeeId(Long idAccount) throws ApiAbacusException, IOException {

		List<Long> lst = new ArrayList<>();
		try {
			// load query
			String query = environment.getProperty("query.select.cuFeeId");
			logger.info("findAccountGlList in given idAccount= {}", idAccount);

			// init params findAccountGl categoryId = 8 for Expenses Account GL (select * from
			// Category) and given branchId
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("idAccount", idAccount);
			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters, new RowMapper<Long>() {
				@Override
				public Long mapRow(ResultSet rs, int rowNum) throws SQLException {

					return rs.getLong("CUFeeID");
				}
			}).get(0);
		}
		catch (IndexOutOfBoundsException e) {
			// e.printStackTrace();
			logger.info("this loan not have a fees");
			return new Long(0);

		}
		catch (Exception e) {
			String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
			if (e.getMessage().contains("errorMessage")) {
				String msgFromTransversApi = e.getMessage().substring(e.getMessage().indexOf('{'));
				final JsonNode jsonNode = new ObjectMapper().readTree(msgFromTransversApi);
				messageError = jsonNode.get("errorMessage").asText();
				logger.error(e.getMessage());
				throw new ApiAbacusException(CommonErrorCode.API_ABACUS,
						jsonNode.get("errorMessage").asText());
			}

			// TODO: handle exception
		}
		return null;

	}

}
