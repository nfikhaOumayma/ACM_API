/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.proxyservice.filter;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.netflix.zuul.filters.support.FilterConstants;
import org.springframework.stereotype.Component;

import com.netflix.zuul.ZuulFilter;
import com.netflix.zuul.context.RequestContext;
import com.netflix.zuul.exception.ZuulException;

/**
 * {@link LogFilter} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
@Component
public class LogFilter extends ZuulFilter {

	/** The logger. */
	Logger logger = LoggerFactory.getLogger(this.getClass());

	/*
	 * (non-Javadoc)
	 * @see com.netflix.zuul.ZuulFilter#filterType()
	 */
	@Override
	public String filterType() {

		// 4 possibilités :
		// pre : permet d'exécuter du code avant la redirection de la requête vers sa destination
		// finale.
		// post : permet d'exécuter du code après que la requête a été redirigée.
		// route : permet d'agir sur la façon de rediriger les requêtes.
		// error : permet d'agir en cas d'erreur lors de la redirection de la requête.
		return FilterConstants.PRE_TYPE;
	}

	/*
	 * (non-Javadoc)
	 * @see com.netflix.zuul.ZuulFilter#filterOrder()
	 */
	@Override
	public int filterOrder() {

		return 1;
	}

	/*
	 * (non-Javadoc)
	 * @see com.netflix.zuul.IZuulFilter#shouldFilter()
	 */
	@Override
	public boolean shouldFilter() {

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see com.netflix.zuul.IZuulFilter#run()
	 */
	@Override
	public Object run() throws ZuulException {

		HttpServletRequest request = RequestContext.getCurrentContext().getRequest();
		logger.debug("**** URL CALLED : [{}] ****", request.getRequestURL());
		return null;
	}
}
