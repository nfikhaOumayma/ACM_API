/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.UDFSettingAbacusService;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;

/**
 * This class @{link LoadDataAddressController}.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataUDFController {

	/** The udf setting abacus service. */
	@Autowired
	private UDFSettingAbacusService udfSettingAbacusService;

	/**
	 * Find user defined fields.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/udf/find-udf-fields")
	public List<UserDefinedFieldsDTO> findUserDefinedFields() {

		return udfSettingAbacusService.findUserDefinedFields();
	}

	/**
	 * Find user defined field group.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/udf/find-udf-group")
	public List<UserDefinedFieldGroupDTO> findUserDefinedFieldGroup() {

		return udfSettingAbacusService.findUserDefinedFieldGroup();
	}

	/**
	 * Find user defined field list values.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/udf/find-udf-list-values")
	public List<UserDefinedFieldListValuesDTO> findUserDefinedFieldListValues() {

		return udfSettingAbacusService.findUserDefinedFieldListValues();
	}

	/**
	 * Load UDF for loan.
	 *
	 * @author HaythemBenizid
	 * @param limite the limite
	 * @return the list
	 */
	@GetMapping("/udf/load-udf-loan/{limite}")
	public List<UserDefinedFieldsLinksDTO> loadUDFForLoan(@PathVariable("limite") Long limite) {

		return udfSettingAbacusService.loadUDFForLoan(limite);
	}

	/**
	 * Load UDF by loan (idAccountExtern).
	 * 
	 * @author HaythemBenizid
	 * @param idAccountExtern the id account extern
	 * @return the list
	 */
	@GetMapping("/udf/load-udf-by-loan/{idAccountExtern}")
	public List<UserDefinedFieldsLinksDTO> loadUDFByLoan(
			@PathVariable("idAccountExtern") Long idAccountExtern) {

		return udfSettingAbacusService.loadUDFByLoan(idAccountExtern);
	}

	/**
	 * Load UDF for customer.
	 *
	 * @author HaythemBenizid
	 * @param limite the limite
	 * @return the list
	 */
	@GetMapping("/udf/load-udf-customer/{limite}")
	public List<UserDefinedFieldsLinksDTO> loadUDFForCustomer(@PathVariable("limite") Long limite) {

		return udfSettingAbacusService.loadUDFForCustomer(limite);
	}

	/**
	 * Load UDF by customer.
	 *
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/udf/load-udf-by-customer/{idCustomer}")
	public List<UserDefinedFieldsLinksDTO> loadUDFByCustomer(
			@PathVariable("idCustomer") Long idCustomer) {

		return udfSettingAbacusService.loadUDFByCustomer(idCustomer);
	}
}
