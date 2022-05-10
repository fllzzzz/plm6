import { createRouter, createWebHistory } from 'vue-router'
import { isNotBlank } from '@data-type/index'
import Layout from '@/layout/index.vue'
import { specialPath } from '@/settings/config'

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
  {
    path: specialPath.QR_SCAN_ARTIFACT_TASK,
    component: () => import('@/views/qr-scan-info/mes/artifact'),
    meta: { title: '构件信息' },
    hidden: true
  },
  {
    path: specialPath.QR_SCAN_ENCLOSURE_TASK,
    component: () => import('@/views/qr-scan-info/mes/enclosure'),
    meta: { title: '围护信息' },
    hidden: true
  },
  {
    path: specialPath.QR_SCAN_AUXILIARY_MATERIAL,
    component: () => import('@/views/qr-scan-info/mes/auxiliary-material'),
    meta: { title: '配套件信息' },
    hidden: true
  },
  {
    path: '/personal',
    component: Layout,
    hidden: true,
    redirect: 'noRedirect',
    children: [
      {
        path: 'center',
        component: () => import('@/views/user-center/index'),
        name: '个人中心',
        meta: { title: '个人中心' }
      }
    ]
  },
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
