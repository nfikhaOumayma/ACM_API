/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.CodeSettingExistException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmEnvironnementService;
import com.acm.utils.dtos.AcmEnvironnementDTO;

/**
 * {@link AcmEnvironnementController} class.
 *
 * @author HaythemBenizid
 * @since 0.11.0
 */
@RestController
@RequestMapping("/acm-environnements")
public class AcmEnvironnementController {

	/** The acmEnvironnement service. */
	@Autowired
	private AcmEnvironnementService acmEnvironnementService;

	/**
	 * Find by key.
	 *
	 * @author HaythemBenizid
	 * @param key the key
	 * @return the acmEnvironnement DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{key}")
	public AcmEnvironnementDTO find(@PathVariable("key") String key)
			throws ResourcesNotFoundException {

		return acmEnvironnementService.find(key);
	}

	/**
	 * update Limite by given key.
	 *
	 * @author HaythemBenizid
	 * @param key the key
	 * @param limite the limite
	 * @return the acm environnement DTO
	 */
	@GetMapping("/updateLimite/{key}/{limite}")
	public AcmEnvironnementDTO updateLimite(@PathVariable("key") String key,
			@PathVariable("limite") String limite) {

		return acmEnvironnementService.updateLimite(key, limite);
	}

	/**
	 * Find by given params.
	 * 
	 * @author HaythemBenizid
	 * @param acmEnvironnementDTO the acmEnvironnement DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<AcmEnvironnementDTO> find(@RequestBody AcmEnvironnementDTO acmEnvironnementDTO) {

		return acmEnvironnementService.find(acmEnvironnementDTO);
	}

	/**
	 * Find by LIKE given KEY.
	 * 
	 * @author HaythemBenizid
	 * @param acmEnvironnementDTO the acmEnvironnement DTO
	 * @return the list
	 */
	@PostMapping("/find-like-key")
	public List<AcmEnvironnementDTO> findLikeKey(
			@RequestBody AcmEnvironnementDTO acmEnvironnementDTO) {

		return acmEnvironnementService.findLikeKey(acmEnvironnementDTO);
	}

	/**
	 * Find like key with token.
	 *
	 * @param acmEnvironnementDTO the acm environnement DTO
	 * @param token the token
	 * @return the list
	 */
	@PostMapping("/find-like-key-with-token")
	public List<AcmEnvironnementDTO> findLikeKeyWithToken(
			@RequestBody AcmEnvironnementDTO acmEnvironnementDTO,
			@RequestHeader("Authorization") String token) {

		return acmEnvironnementService.findLikeKey(acmEnvironnementDTO);
	}

	/**
	 * Create the AcmEnvironnement.
	 *
	 * @author YesserSomai
	 * @param acmEnvironnementDTO the acmEnvironnement DTO
	 * @return the acmEnvironnement DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CodeSettingExistException the code setting exist exception
	 */
	@PostMapping("/create")
	public AcmEnvironnementDTO create(@RequestBody AcmEnvironnementDTO acmEnvironnementDTO)
			throws ResourcesNotFoundException, CodeSettingExistException {

		return acmEnvironnementService.save(acmEnvironnementDTO);
	}

	/**
	 * Update the the AcmEnvironnement.
	 *
	 * @author YesserSomai
	 * @param acmEnvironnementDTO the AcmEnvironnement DTO
	 * @return the AcmEnvironnement DTO
	 * @throws CodeSettingExistException the code setting exist exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public AcmEnvironnementDTO update(@RequestBody AcmEnvironnementDTO acmEnvironnementDTO)
			throws CodeSettingExistException, ResourcesNotFoundException {

		return acmEnvironnementService.save(acmEnvironnementDTO.getId(), acmEnvironnementDTO);
	}

	/**
	 * Find {@link List} of {@link AcmEnvironnementDTO} list.
	 * 
	 * @author YesserSomai
	 * @return the list
	 */
	@GetMapping("/find-all")
	public List<AcmEnvironnementDTO> findSettingMotifRejetss() {

		return acmEnvironnementService.find();
	}

	/**
	 * Find setting I-score.
	 *
	 * @author ManelLamloum
	 * @return the list
	 */
	@GetMapping("/find-setting-iscore")
	public List<AcmEnvironnementDTO> findSettingIScore() {

		return acmEnvironnementService
				.find(new AcmEnvironnementDTO("TAMKEEN_API_REQUEST", Boolean.TRUE));
	}

	/**
	 * Find by category.
	 *
	 * @author Ines Dridi
	 * @param category the category
	 * @return the acm environnement DTO
	 */
	@GetMapping("find-by-category/{category}")
	public List<AcmEnvironnementDTO> findByCategory(@PathVariable("category") String category) {

		return acmEnvironnementService.findByCategory(category);
	}

	/**
	 * Find by keys.
	 *
	 * @author Ines Dridi
	 * @param keys the keys
	 * @return the list
	 */
	@PostMapping("/find-by-keys")
	public List<AcmEnvironnementDTO> findByKeys(@RequestBody List<String> keys) {

		return acmEnvironnementService.findByKeys(keys);
	}

	/**
	 * Check if the connected user group is authorized by acm environnement key.
	 *
	 * @author idridi
	 * @param acmEnvironnementKey the acm environnement key
	 * @return the boolean
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("check-authorisation-connected-user/{acmEnvironnementKey}")
	public Boolean checkAthorisationConnectedUser(
			@PathVariable("acmEnvironnementKey") String acmEnvironnementKey)
			throws ResourcesNotFoundException {

		return acmEnvironnementService.checkAthorisationConnectedUser(acmEnvironnementKey);
	}

}
