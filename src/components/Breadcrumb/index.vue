<template>
  <el-breadcrumb class="app-breadcrumb" separator="/">
    <transition-group name="breadcrumb">
      <template v-for="(item, index) in levelList">
        <el-breadcrumb-item v-if="device !== 'mobile' || index == levelList.length - 1" :key="index">
          <span v-if="item.redirect === 'noRedirect' || index == levelList.length - 1" class="no-redirect">{{ item.meta.title }}</span>
          <a v-else @click.prevent="handleLink(item)">{{ item.meta.title }}</a>
        </el-breadcrumb-item>
      </template>
    </transition-group>
  </el-breadcrumb>
</template>

<script>
import { ElBreadcrumb, ElBreadcrumbItem } from 'element-plus'
import { mapGetters } from 'vuex'
// 路由匹配
import { compile } from 'path-to-regexp'

export default {
  components: { ElBreadcrumb, ElBreadcrumbItem },
  data() {
    return {
      levelList: null
    }
  },
  computed: {
    ...mapGetters(['currentMenu', 'device'])
  },
  watch: {
    $route(route) {
      // if you go to the redirect page, do not update the breadcrumbs
      if (route.path.startsWith('/redirect/')) {
        return
      }
      this.getBreadcrumb()
    }
  },
  created() {
    this.getBreadcrumb()
  },
  methods: {
    getBreadcrumb() {
      // only show routes with meta.title
      let matched = this.$route.matched.filter(item => item.meta && item.meta.title)
      // TODO: 待删除
      // const first = matched[0]

      // if (!this.isDashboard(first)) {
      //   matched = [{ path: '/dashboard', meta: { title: '首页' }}].concat(matched)
      // }
      if (this.currentMenu) {
        matched = [
          { path: `${this.currentMenu.redirect}`, redirect: `${this.currentMenu.redirect}`, meta: { title: `${this.currentMenu.name}` }}
        ].concat(matched)
      }
      this.levelList = matched.filter(item => item.meta && item.meta.title && item.meta.breadcrumb !== false)
    },
    isDashboard(route) {
      const name = route && route.name
      if (!name) {
        return false
      }
      return name.trim().toLocaleLowerCase() === 'Dashboard'.toLocaleLowerCase()
    },
    pathCompile(path) {
      // To solve this problem https://github.com/PanJiaChen/vue-element-admin/issues/561
      const { params } = this.$route
      var toPath = compile(path)
      return toPath(params)
    },
    handleLink(item) {
      const { redirect, path } = item
      if (redirect) {
        this.$router.push(redirect)
        return
      }
      this.$router.push(this.pathCompile(path))
    }
  }
}
</script>

<style lang="scss" scoped>
.app-breadcrumb.el-breadcrumb {
  display: inline-block;
  font-size: 14px;
  line-height: 50px;
  margin-left: 8px;

  .no-redirect {
    color: #97a8be;
    cursor: text;
  }
}
</style>
