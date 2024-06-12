/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.ProductService;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.validation.ACMValidationUtils;

import feign.FeignException;

/**
 * {@link ProductWriter} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class ProductWriter implements ItemWriter<ProductDTO> {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(ProductWriter.class);

	/** The product service. */
	@Autowired
	private ProductService productService;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemWriter#write(java.util.List)
	 */
	@Override
	public void write(List<? extends ProductDTO> productDTOs) throws Exception {

		logger.debug("### ProductWriter : list size = {}", productDTOs.size());
		if (!ACMValidationUtils.isNullOrEmpty(productDTOs)) {
			// saving data in DB
			saveProducts(productDTOs);
		}
		logger.debug("processing [{}]  products in ACM-DB :: DONE", productDTOs.size());
	}

	/**
	 * Save products.
	 *
	 * @author HaythemBenizid
	 * @param productDTOs the product DT os
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void saveProducts(List<? extends ProductDTO> productDTOs)
			throws ResourcesNotFoundException {

		for (ProductDTO productDTO : productDTOs) {
			if (productDTO != null && productDTO.getProductIdAbacus() != null) {
				try {
					// insert imported product into ACM DB
					ProductDTO newProductDTO =
							productService.processByBatch(productDTO, productDTO.getToken());
					logger.debug("{}", newProductDTO);
				}
				catch (FeignException e) {
					logger.error("Failed to save the product in DB");
					logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
							e.getMessage());
				}
			}
		}
		logger.debug("processing products in ACM-DB :: DONE");
	}
}
