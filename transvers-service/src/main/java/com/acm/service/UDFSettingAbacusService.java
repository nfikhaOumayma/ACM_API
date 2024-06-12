/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;

/**
 * {@link UDFSettingAbacusService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public interface UDFSettingAbacusService {

	/**
	 * Find user defined fields.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<UserDefinedFieldsDTO> findUserDefinedFields();

	/**
	 * Find user defined field group.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<UserDefinedFieldGroupDTO> findUserDefinedFieldGroup();

	/**
	 * Find user defined field list values.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<UserDefinedFieldListValuesDTO> findUserDefinedFieldListValues();

	/**
	 * Load UDF from ABACUS DB for loan (cuAccountId) (USED ONLY BY BATCH).
	 *
	 * @author HaythemBenizid
	 * @param limite the limite
	 * @return the list
	 */
	List<UserDefinedFieldsLinksDTO> loadUDFForLoan(Long limite);

	/**
	 * Load UDF for loan by given idAccountExtern.
	 * 
	 * @author HaythemBenizid
	 * @param idAccountExtern the id account extern
	 * @return the list
	 */
	List<UserDefinedFieldsLinksDTO> loadUDFByLoan(Long idAccountExtern);

	/**
	 * Load UDF from ABACUS DB for ID customer (USED ONLY BY BATCH).
	 *
	 * @author HaythemBenizid
	 * @param limite the limite
	 * @return the list
	 */
	List<UserDefinedFieldsLinksDTO> loadUDFForCustomer(Long limite);

	/**
	 * Load UDF by customer ID.
	 * 
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	List<UserDefinedFieldsLinksDTO> loadUDFByCustomer(Long idCustomer);

}
