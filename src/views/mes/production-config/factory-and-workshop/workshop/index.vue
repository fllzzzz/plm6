<template>
  <el-card>
    <template v-slot:header>
      <span style="line-height: 1.7">{{ crud.title }}列表 </span>
      <el-tag v-if="factory && factory.name" type="success">{{ factory.name }}</el-tag>
      <!-- 新增 -->
      <common-button
        v-permission="permission.add"
        :disabled="!factoryId"
        class="filter-item"
        style="float: right; padding: 6px 10px"
        size="mini"
        type="primary"
        icon="el-icon-plus"
        @click="crud.toAdd"
      >
        新增
      </common-button>
    </template>
    <div v-show="!factoryId">
      <div class="my-code">点击工厂查看详情</div>
    </div>
    <div v-show="factoryId">
      <!--工具栏-->
      <div class="head-container">
        <mHeader />
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="columns.visible('name')"
          key="name"
          prop="name"
          :show-overflow-tooltip="true"
          label="车间名称"
          min-width="140px"
        />
        <el-table-column
          v-if="columns.visible('shortName')"
          key="shortName"
          prop="shortName"
          :show-overflow-tooltip="true"
          label="车间简称"
          min-width="140px"
        />
        <el-table-column
          v-if="columns.visible('boolEnabledEnum')"
          key="boolEnabledEnum"
          prop="boolEnabledEnum"
          label="状态"
          align="center"
          width="80px"
        >
          <template v-slot:header>
            <el-tooltip class="item" effect="light" :content="`车间被禁用后，该车间无法再在其他页面中显示`" placement="top">
              <div style="display: inline-block">
                <span>状态</span>
                <i class="el-icon-info" />
              </div>
            </el-tooltip>
          </template>
          <template v-slot="scope">
            <el-switch
              v-model="scope.row.boolEnabledEnum"
              :disabled="!checkPermission(permission.editStatus)"
              active-color="#409EFF"
              inactive-color="#F56C6C"
              :active-value="enabledEnum.TRUE.V"
              :inactive-value="enabledEnum.FALSE.V"
              @change="changeStatus(scope.row, scope.row.boolEnabledEnum)"
            />
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" label="排序" align="center" width="80px" />
        <el-table-column
          v-if="columns.visible('remark')"
          key="remark"
          prop="remark"
          :show-overflow-tooltip="true"
          label="备注"
          min-width="160px"
        />
        <!--编辑与删除-->
        <el-table-column
          v-if="checkPermission([...permission.del, ...permission.edit])"
          label="操作"
          width="130px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <udOperation :data="scope.row" />
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
    </div>
    <mForm />
  </el-card>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/mes/production-config/workshop'
import { ref, defineProps, computed, watch, inject } from 'vue'
import { ElMessageBox } from 'element-plus'
import checkPermission from '@/utils/system/check-permission'

import { enabledEnum } from '@enum-ms/common'

import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

// crud交由presenter持有
const permission = {
  get: ['workshop:get'],
  add: ['workshop:add'],
  edit: ['workshop:edit'],
  del: ['workshop:del'],
  editStatus: ['workshop:editStatus']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '车间',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false,
    invisibleColumns: ['shortName']
  },
  tableRef
)

const maxHeight = inject('maxHeight')
const props = defineProps({
  factory: {
    type: Object,
    default: () => {}
  }
})

const factoryId = computed(() => {
  return props.factory && props.factory.id
})

watch(
  () => factoryId,
  (val) => {
    if (val.value) {
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

async function changeStatus(data, val) {
  try {
    await ElMessageBox.confirm('此操作将 "' + enabledEnum.VL[val] + '" ' + data.name + ', 是否继续？', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus({ id: data.id, boolEnabledEnum: val })
    crud.refresh()
    crud.notify(enabledEnum.VL[val] + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('变更车间状态', error)
    data.boolEnabledEnum = data.boolEnabledEnum === enabledEnum.TRUE.V ? enabledEnum.FALSE.V : enabledEnum.TRUE.V
  }
}

CRUD.HOOK.beforeToQuery = () => {
  crud.query.factoryId = factoryId
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.factoryId = factoryId
}
</script>

<style lang="scss" scoped>
.color-card {
  display: inline-block;
  width: 50px;
  height: 20px;
  vertical-align: middle;
}
</style>
