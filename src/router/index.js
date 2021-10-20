import { createRouter, createWebHistory } from 'vue-router'
import { isNotBlank } from '@/utils/data-type'
import Layout from '@/layout/index.vue'

/**
 * constantRoutes
 * a base page that does not have permission requirements
 * all roles can be accessed
 */
const constantRoutes = [
  {
    path: '/redirect',
    component: Layout,
    hidden: true,
    children: [
      {
        path: '/redirect/:path*',
        component: () => import('@/views/redirect/index')
      }
    ]
  },
  { // TODO:projectType 用处
    path: '/login',
    component: () => import('@/views/login/index'),
    meta: { projectType: 0 },
    hidden: true
  },
  {
    path: '/',
    component: () => import('@/views/login/index'),
    meta: { projectType: 0 },
    hidden: true
  },
  // {
  //   path: '/personal',
  //   component: () => import('@/views/user-center/index'),
  //   meta: { title: '个人中心' },
  //   hidden: true
  // },
  {
    path: '/auth-redirect',
    component: () => import('@/views/login/auth-redirect'),
    hidden: true
  },
  {
    path: '/401',
    component: () => import('@/views/error-page/401'),
    hidden: true
  },
  {
    path: '/404',
    component: () => import('@/views/error-page/404'),
    hidden: true
  }
  // {
  //   path: '/',
  //   component: Layout,
  //   redirect: '/dashboard',
  //   children: [
  //     {
  //       path: 'dashboard',
  //       component: () => import('@/views/dashboard/index'),
  //       name: 'Dashboard',
  //       meta: { title: '首页', icon: 'dashboard', affix: true }
  //     }
  //   ]
  // }
]

const router = createRouter({
  history: createWebHistory(),
  routes: constantRoutes
})

// 重置路由
export const resetRouter = () => {
  const newRouter = createRouter({
    history: createWebHistory(),
    routes: constantRoutes
  })
  router.matcher = newRouter.matcher // reset router
}

// 添加路由
export const addRoutes = (asyncRoutes = [], parentName) => {
  asyncRoutes.forEach(route => {
    if (parentName) {
      router.addRoute(parentName, route)
    } else {
      router.addRoute(route)
    }
    if (isNotBlank(route.name) && isNotBlank(route.children)) {
      addRoutes(route.children, route.name)
    }
  })
}

export {
  constantRoutes
}

export default router
