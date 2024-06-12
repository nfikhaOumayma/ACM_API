package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.ChargeFeesDTO;

/**
 * The Class ChargeFeesService.
 */
public interface ChargeFeesService {

	/**
	 * Find.
	 *
	 * @param chargeFeesDTO the charge fees DTO
	 * @return the list
	 */
	List<ChargeFeesDTO> find(ChargeFeesDTO chargeFeesDTO);

	/**
	 * Save.
	 *
	 * @param chargeFeesDTO the charge fees DTO
	 * @return the charge fees DTO
	 */
	ChargeFeesDTO save(ChargeFeesDTO chargeFeesDTO);

	/**
	 * Save all.
	 *
	 * @param chargeFeesDTO the charge fees DTO
	 * @return the list
	 */
	List<ChargeFeesDTO> saveAll(List<ChargeFeesDTO> chargeFeesDTO);

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param chargeFeesDTO the charge fees DTO
	 * @return the charge fees DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ChargeFeesDTO save(Long id, ChargeFeesDTO chargeFeesDTO) throws ResourcesNotFoundException;

	/**
	 * Delete.
	 *
	 * @param id the id
	 */
	void delete(long id);

}
