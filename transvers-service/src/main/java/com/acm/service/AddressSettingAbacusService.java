/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.AddressListDTO;
import com.acm.utils.dtos.AddressListValueDTO;
import com.acm.utils.dtos.AddressSettingAbacusDTO;
import com.acm.utils.dtos.AddressTypeDTO;

/**
 * {@link AddressSettingAbacusService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public interface AddressSettingAbacusService {

	/**
	 * Find {@link List} of {@link AddressTypeDTO} data.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<AddressTypeDTO> findAddressType();

	/**
	 * Find address list.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<AddressListDTO> findAddressList();

	/**
	 * Find address list value.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<AddressListValueDTO> findAddressListValue();

	/**
	 * Find settings address.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<AddressSettingAbacusDTO> findSettingsAddress();

	/**
	 * Load Address from ABACUS DB for customer (USED ONLY BY BATCH).
	 *
	 * @author HaythemBenizid
	 * @param limite the limite
	 * @return the list
	 */
	List<AddressDTO> loadAddressForCustomer(Long limite);

	/**
	 * Load address by customer.
	 * 
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	List<AddressDTO> loadAddressByCustomer(Long idCustomer);
}
