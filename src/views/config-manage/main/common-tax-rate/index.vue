<template>
  <div class="app-container">
    <el-card shadow="always" class="common-tax-rate">
      <template #header>
        <div class="clearfix">
          <span class="card-title">常用税率配置</span>
        </div>
      </template>
      <common-table ref="tableRef" v-loading="crud.loading" :data="crud.data" row-key="name">
        <el-table-column
          v-if="columns.visible('name')"
          key="name"
          :show-overflow-tooltip="true"
          prop="name"
          label="种类"
          align="center"
          width="120"
        />
        <el-table-column
          v-if="columns.visible('taxRate')"
          key="taxRate"
          :show-overflow-tooltip="true"
          prop="taxRate"
          label="税率"
          align="left"
        />
        <!--编辑与删除-->
        <el-table-column v-permission="permission.edit" label="操作" width="100px" align="center" fixed="right">
          <template v-slot="scope">
            <udOperation :data="scope.row" :show-del="false" />
          </template>
        </el-table-column>
      </common-table>
    </el-card>
    <m-form />
  </div>
</template>

<script setup>
import crudApi from '@/api/config/wms/tax-rate'
import { configCommonTaxRatePM as permission } from '@/page-permission/config'

import { ref } from 'vue'
import { supplierClassEnum } from '@enum-ms/supplier'

import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mForm from './module/form.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()

const { CRUD, crud, columns } = useCRUD(
  {
    title: '税率设定',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

CRUD.HOOK.handleRefresh = (crud, { data: { content = [] }}) => {
  content.forEach((v) => {
    v.name = supplierClassEnum.VL[v.classification]
    v.taxRate = v.taxRateList.map((v) => `${v}%`).join('、')
  })
}
</script>

<style lang="scss" scoped>
.common-tax-rate {
  width: 500px;
}
</style>
