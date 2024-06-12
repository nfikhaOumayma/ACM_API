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
import com.acm.model.rowmapper.ProductDTOAbacusRowMapper;
import com.acm.model.rowmapper.ProductDTOAnalyticsRowMapper;
import com.acm.service.ProductAbacusServices;
import com.acm.utils.dtos.ProductDTO;

/**
 * {@link ProductAbacusServiceImpl} class.
 *
 * @author YesserSomai
 * @since 0.11.0
 */
@Service
public class ProductAbacusServiceImpl implements ProductAbacusServices {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(ProductAbacusServiceImpl.class);

	/** The environment. */
	@Autowired
	private Environment environment;

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductAbacusServices#find(java.lang.Integer)
	 */
	@Override
	public ProductDTO find(Long productId) {

		logger.info("Get Product with ID = [{}]", productId);
		try {
			// load query
			String query = environment.getProperty("query.select.product.term.balance");
			logger.info("find by productId= {}", productId);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("id_product", productId);
			// execute query
			return namedParameterJdbcTemplate.queryForObject(query, namedParameters,
					new ProductDTOAnalyticsRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.product.term.balance ### {}",
					e.getMessage());
		}
		return new ProductDTO();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductAbacusServices#find()
	 */
	@Override
	public List<ProductDTO> find() {

		try {
			// load query
			String query = environment.getProperty("query.select.all.product");
			// execute query
			return namedParameterJdbcTemplate.query(query, new ProductDTOAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.all.product ### {}", e.getMessage());
		}
		return new ArrayList<>();
	}
}
