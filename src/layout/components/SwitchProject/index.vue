<template>
  <div v-show="showable" class="cascader-container">
    <common-select
      v-model:value="projectType"
      :options="projectTypeEnum"
      :all-val="allVal"
      :disabled-val="disabledTypeArr"
      show-all
      type="enum"
      size="medium"
      placeholder="项目类型"
      class="project-type-select"
      @change="handleTypeChange"
    />
    <el-cascader
      :key="cascaderKey"
      v-model="currentProjectId"
      placeholder="可搜索：项目名称"
      :options="projectsCascade"
      :props="props"
      :show-all-levels="false"
      size="medium"
      filterable
      style="width: 400px"
      @change="projectChange"
    />
    <el-tooltip class="item" effect="dark" content="刷新项目列表" placement="right">
      <i v-if="!refreshLoading" class="el-icon-refresh" style="cursor: pointer" @click="fetchProjectYearCascade" />
      <i v-else class="el-icon-loading" />
    </el-tooltip>
  </div>
</template>

<script>
import { mapGetters } from 'vuex'
import { getUserProjectGroupByYear } from '@/api/contract/project'
import enumOperate from '@/utils/enum'
import { projectTypeEnum } from '@/utils/enum/modules/contract'
import { getBitwiseBack } from '@/utils/data-type/number'
const projectTypes = enumOperate.toArr(projectTypeEnum).map((e) => e.V)
const allVal = 0
projectTypes.push(allVal)
// const loginPath = ['/login', '/']

export default {
  data() {
    return {
      // permission,
      allVal, // 全部时的值
      cascaderKey: 0,
      projectTypeEnum,
      projectType: undefined,
      currentProjectId: undefined,
      projectCascadeList: [],
      refreshLoading: false,
      currentComponentChange: false,
      props: {
        value: 'id',
        label: 'label',
        children: 'children',
        expandTrigger: 'hover',
        emitPath: false
      }
    }
  },
  computed: {
    ...mapGetters(['globalProjectId', 'projectsCascade', 'routeProjectType', 'currentMenu', 'currentProjectType']),
    visitedViews() {
      return this.$store.state.tagsView.visitedViews
    },
    showable() {
      return this.$isNotBlank(this.routeProjectType)
    },
    disabledTypeArr() {
      const types = []
      if (this.routeProjectType !== allVal) {
        projectTypes.forEach((v) => {
          if (!(v & this.routeProjectType)) {
            types.push(v)
          }
        })
      }
      return types
    }
  },
  watch: {
    routeProjectType: {
      handler(newVal, oldVal) {
        if (newVal && !(newVal & this.projectType)) {
          const valArr = getBitwiseBack(this.routeProjectType)
          this.projectType = valArr.length && valArr.length <= 1 ? newVal : newVal & this.currentProjectType ? this.currentProjectType : valArr[0]
          this.handleTypeChange(this.projectType)
        }
      }
    },
    globalProjectId(val) {
      // TODO:关于跳转界面待定，mes进入管理会触发, 该判断待定
      if (!this.currentComponentChange) {
        this.currentProjectId = val
        // if (val && loginPath.indexOf(this.$route.path) === -1) {
        //   this.refreshSelectedTag()
        // } else {
        //   this.closeAllTags()
        // }
      }
    }
  },
  mounted() {
    this.currentProjectId = this.globalProjectId
    this.projectType = this.currentProjectType
    this.$nextTick(() => {
      if (this.routeProjectType !== allVal && this.projectType === 0) {
        this.projectType = undefined
      }
    })
    if (!this.projectsCascade || this.projectsCascade.length === 0) {
      this.fetchProjectYearCascade()
    }
  },
  methods: {
    async handleTypeChange(val) {
      try {
        ++this.cascaderKey
        await this.$store.dispatch('project/changeProjectType', val)
      } catch (error) {
        // TODO: 失败应切换回上一个类型
        console.log('切换项目', error)
      }
    },
    async projectChange(val) {
      this.currentComponentChange = true
      try {
        await this.$store.dispatch('project/setProjectId', val)
        // if (val && loginPath.indexOf(this.$route.path) === -1) {
        //   this.refreshSelectedTag()
        // } else {
        //   this.closeAllTags()
        // }
      } catch (error) {
        console.log(error)
      } finally {
        this.currentComponentChange = false
      }
    },
    /**
     * 获取项目年份级联列表
     */
    async fetchProjectYearCascade() {
      try {
        this.refreshLoading = true
        const { content = [] } = (await getUserProjectGroupByYear()) || {}
        const projects = content
        await this.$store.dispatch('project/setProjects', projects)
        if (this.routeProjectType && !(this.routeProjectType & this.projectType)) {
          const routeProjectTypeArr = getBitwiseBack(this.routeProjectType)
          this.projectType =
            routeProjectTypeArr.length && routeProjectTypeArr.length <= 1
              ? this.routeProjectType
              : this.routeProjectType & this.currentProjectType
                ? this.currentProjectType
                : undefined
          this.handleTypeChange(this.projectType)
        }
      } catch (error) {
        console.log(error)
        this.$message.error('获取项目级联列表失败')
      } finally {
        this.refreshLoading = false
      }
    },
    getActive() {
      let view
      for (const route of this.visitedViews) {
        if (route.path === this.$route.path) {
          view = route
        }
      }
      return view
    },
    refreshSelectedTag() {
      const view = this.getActive()
      if (view) {
        // TODO: 检查view不存在的原因
        this.closeOthersTags(view)
        this.$store.dispatch('tagsView/delCachedView', view).then(() => {
          const { fullPath } = view
          this.$nextTick(() => {
            this.$router.replace({
              path: '/redirect' + fullPath
            })
          })
        })
      }
    },
    closeOthersTags(selectedTag) {
      this.$router.push(selectedTag)
      this.$store.dispatch('tagsView/delOthersViews', selectedTag)
    },
    closeAllTags() {
      this.$store.dispatch('tagsView/delAllViews').then(() => {
        // if (this.currentMenu && this.currentMenu.redirect) {
        //   this.$router.push({ path: this.currentMenu.redirect })
        // }
      })
    }
  }
}
</script>

<style lang="scss" scoped>
.cascader-container {
  display: inline-flex;
  height: 100%;
  align-items: center;
  padding: 10px;
  color: cornflowerblue;
  .project-type-select {
    width: 100px;
  }
  ::v-deep(.el-input input) {
    border: none;
    color: cornflowerblue;
    background-color: unset;
  }
}
</style>
