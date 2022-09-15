<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="headerRef" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60"/>
      <el-table-column v-if="columns.visible('signDate')" key="signDate" prop="signDate" :show-overflow-tooltip="true" label="签订日期" align="center" />
      <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="订单号" align="center" />
      <el-table-column v-if="columns.visible('project')" key="project" prop="project" :show-overflow-tooltip="true" label="项目" align="center" min-width="180" />
      <el-table-column v-if="columns.visible('supplierName')" key="supplierName" prop="supplierName" :show-overflow-tooltip="true" label="分包单位" align="center"  />
      <el-table-column v-if="columns.visible('subcontractClassName')" key="subcontractClassName" prop="subcontractClassName" :show-overflow-tooltip="true" label="分包类别" align="center" />
      <el-table-column v-if="columns.visible('amount')" key="amount" prop="amount" :show-overflow-tooltip="true" label="合同额" align="center" />
      <el-table-column v-if="columns.visible('userName')" key="userName" prop="userName" :show-overflow-tooltip="true" label="创建人" align="center" />
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.detail,...permission.edit])"
        label="操作"
        width="180px"
        align="center"
      >
        <template #default="{ row }">
          <udOperation
            :data="row"
            :permission="permission"
            show-detail
          />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
    <mDetail />
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/subcontract-manage/subcontract-order'
import { ref } from 'vue'

import { subcontractOrderPM as permission } from '@/page-permission/supply-chain'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail'
import udOperation from '@crud/UD.operation.vue'

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const headerRef = ref()
const dataFormat = ref([
  ['signDate', ['parse-time', '{y}-{m}-{d}']],
  ['project', 'parse-project'],
  ['amount', 'to-thousand']
])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '分包订单',
    sort: [],
    permission: { ...permission },
    crudApi: crudApi,
    invisibleColumns: [],
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

// 刷新数据后
CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    const monomerIds = []
    v.monomerList.map(v => {
      monomerIds.push(v.id)
    })
    v.monomerIds = monomerIds
    return v
  })
}

</script>
<style lang="scss" scoped>
.clickable {
  cursor: pointer;
}
</style>
