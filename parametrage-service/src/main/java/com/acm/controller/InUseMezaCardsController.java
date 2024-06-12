/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.constants.common.CommonFunctions;

/**
 * The Class InUseMezaCardsController.
 */
@RestController
@RequestMapping("/in-use-meza-cards")
public class InUseMezaCardsController {

	/**
	 * Find and add if not exist.
	 *
	 * @author ManelLamloum
	 * @param login the login
	 * @param mezaCardNumber the meza card number
	 * @return the boolean
	 */
	@GetMapping("/find-and-add-if-not-exist/{mezaCardNumber}/{login}")
	public Boolean findAndAddIfNotExist(@PathVariable("login") String login,
			@PathVariable("mezaCardNumber") String mezaCardNumber) {

		if (Boolean.TRUE.equals(CommonFunctions.inUseMezaCardsContains(mezaCardNumber))) {
			return true;
		}
		CommonFunctions.deleteFromInUseMezaCards(login);
		CommonFunctions.addInUseMezaCard(login, mezaCardNumber);
		return false;
	}

	/**
	 * Delete.
	 *
	 * @param login the login
	 */
	@DeleteMapping("/delete/{login}")
	public void delete(@PathVariable("login") String login) {

		CommonFunctions.deleteFromInUseMezaCards(login);
	}
}
