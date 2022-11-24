<template>
  <div class="app-container">
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column
        v-if="columns.visible('productType')"
        key="productType"
        prop="productType"
        :show-overflow-tooltip="true"
        label="类型"
        width="140px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('processSequence')"
        key="processSequence"
        prop="processSequence"
        :show-overflow-tooltip="true"
        label="工序"
        min-width="160px"
      />
      <!--编辑与删除-->
      <el-table-column v-if="checkPermission([...permission.edit])" label="操作" width="130px" align="center" fixed="right">
        <template #default="{ row: { sourceRow: row } }">
          <udOperation :disabledEdit="row.productType === 1" :data="row" :showDel="false" />
        </template>
      </el-table-column>
    </common-table>
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-config/process'
import { ref } from 'vue'
import { useStore } from 'vuex'

import { bridgeProcessTypeEnum as typeEnum } from '@enum-ms/bridge'
// import { processMaterialListTypeEnum as typeEnum } from '@enum-ms/mes'
import checkPermission from '@/utils/system/check-permission'
import { configProcessPM as permission } from '@/page-permission/config'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mForm from './module/form'
import { deepClone } from '@/utils/data-type'

const store = useStore()

const dataFormat = [['productType', ['parse-enum', typeEnum, { bit: true }]]]

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '工序',
    sort: [],
    permission: { ...permission },
    hasPagination: false,
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight()
const dataPath = {
  // [typeEnum.ARTIFACT.K]: 'artifactProcessList',
  // [typeEnum.ASSEMBLE.K]: 'assembleProcessList',
  // [typeEnum.MACHINE_PART.K]: 'machinePartProcessList'
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content = []
  for (const item in dataPath) {
    const _list = data[dataPath[item]]
    const process = deepClone(_list)
    let processSequence = ''
    if (process && process.length > 0) {
      processSequence = process.map((v) => `【${v.name}】`).join('')
    } else {
      processSequence = ''
    }
    data.content.push({
      productType: typeEnum.KV[item],
      list: _list,
      source: deepClone(_list),
      processSequence: processSequence
    })
  }
}

// 编辑之后 取消缓存的已加载设置
CRUD.HOOK.afterSubmit = () => {
  store.commit('config/SET_LOADED', { key: 'process', loaded: false })
}
CRUD.HOOK.afterDelete = () => {
  store.commit('config/SET_LOADED', { key: 'process', loaded: false })
}
</script>
