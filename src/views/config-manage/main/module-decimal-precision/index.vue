<template>
  <div class="app-container">
    <el-card shadow="always" class="common-tax-rate">
      <template #header>
        <div class="clearfix">
          <span class="card-title">模块小数精度配置</span>
        </div>
      </template>
      <div style="margin-bottom:10px;">
        <!-- <common-button type="success" @click="crud.toAdd" v-permission="permission.add">全局配置</common-button> -->
        <common-button type="primary" @click="crud.toAdd" v-permission="permission.add">添加</common-button>
      </div>
      <common-table ref="tableRef" v-loading="crud.loading" :max-height="maxHeight" :data="crud.data" row-key="name">
        <el-table-column
          v-if="columns.visible('menuName')"
          key="menuName"
          :show-overflow-tooltip="true"
          prop="menuName"
          label="模块"
          align="center"
        />
        <el-table-column
          v-if="columns.visible('type')"
          key="type"
          :show-overflow-tooltip="true"
          prop="type"
          label="小数类型"
          align="center"
          width="120"
        >
        <template #default>
          单价
        </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('scale')"
          key="scale"
          :show-overflow-tooltip="true"
          prop="scale"
          label="小数精度"
          align="left"
        />
        <!--编辑与删除-->
        <el-table-column v-permission="permission.edit" label="操作" width="120px" align="center" fixed="right">
          <template #default="{ row }">
            <udOperation :data="row" :show-edit="false" />
          </template>
        </el-table-column>
      </common-table>
    </el-card>
    <m-form :menuArr="menuArr" :disabledId="disabledId" />
    <mBatchForm :menuArr="menuArr" :disabledId="disabledId" />
  </div>
</template>

<script setup>
import crudApi, { getMenuModule } from '@/api/config/main/module-decimal-precision'
import { moduleDecimalPrecisionPM as permission } from '@/page-permission/config'

import { ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import { isNotBlank } from '@data-type/index'

import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mForm from './module/form.vue'
import mBatchForm from './module/batch-form'

const optShow = {
  batchAdd: true,
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const menuArr = ref([])
const disabledId = ref([])

const { maxHeight } = useMaxHeight({ paginate: false })

const { CRUD, crud, columns } = useCRUD(
  {
    title: '模块小数精度配置',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

fetchMenuModule()

async function fetchMenuModule() {
  menuArr.value = []
  try {
    const { content } = await getMenuModule()
    const filterValue = content.filter(v => (v.name === 'WMS' || v.name === '合同管理'))
    if (isNotBlank(filterValue)) {
      menuArr.value.push(...filterValue)
    }
  } catch (e) {
    console.log(e)
  }
}
CRUD.HOOK.handleRefresh = (crud, { data: { content = [] }}) => {
  disabledId.value = []
  content.forEach((v) => {
    if (disabledId.value.indexOf(v.menuId) < 0) {
      disabledId.value.push(v.menuId)
    }
  })
}
</script>

<style lang="scss" scoped>
.common-tax-rate {
  width: 700px;
}
</style>
