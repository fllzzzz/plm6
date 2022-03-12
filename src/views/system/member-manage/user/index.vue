<template>
  <div class="app-container wrap">
    <div class="wrap-left">
      <div class="head-container">
        <el-input
          v-model.trim="filterText"
          size="small"
          clearable
          placeholder="输入部门名称搜索"
        />
      </div>
      <el-tree
        ref="treeMenuRef"
        v-loading="loading.data"
        :data="treeMenu"
        :props="defaultProps"
        :filter-node-method="filterNode"
        highlight-current
        :expand-on-click-node="false"
        :default-expand-all="true"
        node-key="id"
        @node-click="handleNodeClick"
      >
        <template #default="{ node }">
          <div style="padding: 3px 5px; border-radius: 3px; width: 100%;">
            <span style="font-weight:bold">{{ node.label }}</span>
          </div>
        </template>
      </el-tree>
    </div>
    <div class="wrap-right">
      <m-header
        id="header"
        ref="header"
      />
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        style="width: 100%;margin-left:20px;"
        @selection-change="crud.selectionChangeHandler"
      >
        <el-table-column
          type="selection"
          width="55"
        />
        <el-table-column
          label="序号"
          type="index"
          align="center"
          width="60"
        />
        <el-table-column
          v-if="columns.visible('username')"
          key="username"
          prop="username"
          :show-overflow-tooltip="true"
          width="140"
          label="员工编号"
        />
        <el-table-column
          v-if="columns.visible('name')"
          key="name"
          prop="name"
          :show-overflow-tooltip="true"
          width="100"
          label="姓名"
        />
        <el-table-column
          v-if="columns.visible('sex')"
          key="sex"
          prop="sex"
          label="性别"
          width="100"
          align="center"
        >
          <template v-slot="scope">
            <span>{{ isNotBlank(scope.row.sex)? userSexEnum.VL[scope.row.sex]: '' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('phone')"
          key="phone"
          prop="phone"
          :show-overflow-tooltip="true"
          width="140"
          label="电话"
        />
        <el-table-column
          v-if="columns.visible('email')"
          key="email"
          prop="email"
          :show-overflow-tooltip="true"
          min-width="170"
          label="邮箱"
        />
        <el-table-column
          v-if="columns.visible('deptAndJob')"
          key="deptAndJob"
          prop="deptAndJob"
          :show-overflow-tooltip="true"
          min-width="140"
          label="部门 / 岗位"
        />
        <el-table-column
          key="enabled"
          prop="enabled"
          label="状态"
          align="center"
        >
          <template v-slot="scope">
            <el-switch
              v-model="scope.row.enabled"
              active-color="#409EFF"
              inactive-color="#F56C6C"
              :active-value="enabledEnum.TRUE.V"
              :inactive-value="enabledEnum.FALSE.V"
              @change="changeStatus(scope.row, scope.row.enabled)"
            />
          </template>
        </el-table-column>
        <!--编辑与删除-->
        <el-table-column
          v-if="checkPermission([...permission.edit, ...permission.del])"
          label="操作"
          width="130px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <udOperation
              :data="scope.row"
              :permission="permission"
            />
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
    </div>
    <m-form :dept-tree="treeMenu" />
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/system/member-manage/user'
import { deptTree } from '@/api/system/member-manage/dept'
import { provide, reactive, ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation.vue'
import mHeader from './module/header'
import mForm from './module/form'
import { enabledEnum } from '@enum-ms/common'
import pagination from '@crud/Pagination'
import { ElMessageBox } from 'element-plus'
import { userSexEnum } from '@enum-ms/system'
import { isNotBlank } from '@data-type/index'

const permission = {
  get: ['user:get'],
  add: ['user:add'],
  edit: ['user:edit'],
  del: ['user:del'],
  editStatus: ['user:editStatus']
}

const optShow = {
  add: true,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '用户',
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.user',
  paginate: true,
  extraHeight: 90
})

const treeMenuRef = ref() // 菜单ref
const filterText = ref() // 菜单过滤输入
const treeMenu = ref([]) // 树菜单
const lastCurrentRow = ref({}) // 菜单当前选中节点
const defaultProps = { children: 'children', label: 'name' } // 树结构数据默认格式
const loading = reactive({
  // 加载
  data: false
})

provide('currentNode', lastCurrentRow)

// tree过滤输入监听
watch(filterText, (val) => {
  treeMenuRef.value.filter(val)
})

// 加载数据
fetchDeptTree()

// 拉取部门树
async function fetchDeptTree() {
  try {
    loading.data = true
    treeMenu.value = await deptTree({ hasRoot: true })
  } catch (error) {
    console.log('部门tree', error)
  } finally {
    loading.data = false
  }
}

// 菜单过滤
function filterNode(value, data) {
  if (!value) return true
  return data.name.includes(value)
}

// 切换清单
function handleNodeClick(data) {
  lastCurrentRow.value = data
  treeMenuRef.value.setCurrentKey(lastCurrentRow.value.id, true)
  crud.query.deptId = data.id
  crud.toQuery()
}

async function changeStatus(data, val) {
  try {
    const msg = val === enabledEnum.TRUE.V ? '启用' : '禁用'
    await ElMessageBox.confirm(`确定${msg}“${data.name}” 吗 ？`, '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus({ id: data.id, enabled: val })
    crud.notify(`${msg}成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.refresh()
  } catch (error) {
    console.log(error)
    data.status = data.status === enabledEnum.TRUE.V ? enabledEnum.FALSE.V : enabledEnum.TRUE.V
  }
}
CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map((v) => {
    const deptName = v.dept ? v.dept.name : ''
    const jobName = v.job ? v.job.name : ''
    v.deptAndJob = `${deptName}/${jobName}`
    return v
  })
}
</script>

<style lang="scss" scoped>
.wrap {
  display: flex;
  .wrap-left {
    width: 380px;
    .el-tree {
      overflow-y: auto;
      padding-right: 5px;
      font-size: 15px;
    }
    ::v-deep(.el-tree--highlight-current .el-tree-node.is-current > .el-tree-node__content) {
      background-color: #ffe48d !important;
    }
  }
  .wrap-right {
    flex: 1;
    min-width: 0;
    overflow: hidden;
  }
}
::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}

.card-wrap {
  white-space: nowrap;
  overflow-y: auto;
  // margin-top: 12px;
  // display: flex;
  // flex-wrap: wrap;
  .el-card {
    display: inline-block;
    width: 280px;
    margin-bottom: 10px;
    margin-left: 20px;
  }
}
</style>
