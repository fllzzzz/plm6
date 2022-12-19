<template>
  <div class="app-container">
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
      <el-table-column v-if="columns.visible('name')" prop="name" align="center" :show-overflow-tooltip="true" label="零件类型" />
      <el-table-column v-if="columns.visible('classifyLinkName')" prop="classifyLinkName" :show-overflow-tooltip="true" label="零件科目" min-width="200" />
      <el-table-column v-if="columns.visible('specPrefixList')" key="specPrefixList" prop="specPrefixList" :show-overflow-tooltip="true" label="规格前缀索引" align="center" min-width="200">
        <template v-slot="scope">
          <template v-if="scope.row.specPrefixList && scope.row.specPrefixList.length > 0">
            <span v-for="(item,index) in scope.row.specPrefixList" :key="index">
              {{`【${item}】`}}
            </span>
          </template>
          <div v-else>-</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('productProcessLinkList')" prop="productProcessLinkList" label="工序" min-width="220">
        <template #default="{ row: { sourceRow: row } }">
          <el-tooltip :content="`${row.processSequence}`" placement="top-start">
            <div style="display: flex; align-items: center;flex-wrap: wrap; white-space: nowrap">
              <div style="display: flex; align-items: center" v-for="(item, index) in row.productProcessLinkList" :key="item.id">
                <span>【{{ item.name }}】</span>
                <div
                  style="
                    display: flex;
                    align-items: center;
                    flex-direction: column;
                    height: 35px;
                    width: 25px;
                    text-align: center;
                    justify-content: center;
                    position: relative;
                  "
                  v-if="index !== row.productProcessLinkList.length - 1"
                >
                  <div
                    v-if="item.nodeTime"
                    style="position: absolute; top: -5px; font-weight: 600; left: 44%; transform: translateX(-50%)"
                    class="tc-warning"
                  >
                    {{ item.nodeTime }}
                  </div>
                  <div style="font-size: 25px; margin-top: 1px; position: absolute; top: 50%; transform: translateY(-50%)">→</div>
                </div>
              </div>
            </div>
          </el-tooltip>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('boolSectionSteel')"
        key="boolSectionSteel"
        prop="boolSectionSteel"
        :show-overflow-tooltip="true"
        label="是否型材"
        width="100"
        align="center"
      >
        <template v-slot="scope">
          <span>{{isNotBlank(scope.row.boolSectionSteel)?partClsEnum.VL[scope.row.boolSectionSteel]:'-' }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.edit]" label="操作" width="100px" align="center" fixed="right">
        <template v-slot="scope">
          <udOperation :data="scope.row" :showDel="false" />
          <!-- <udOperation :data="scope.row" :disabledEdit="scope.row.basicClass !== 1" :showDel="false" /> -->
        </template>
      </el-table-column>
    </common-table>
    <mForm />
  </div>
</template>

<script setup>
import crudApi, { getMachinePart } from '@/api/bridge/production-config/product-process'
import { ref } from 'vue'
import { bridgeConfigProductProcessMachinePartPM as permission } from '@/page-permission/config'
import { isNotBlank } from '@data-type/index'
import { partClsEnum } from '@enum-ms/bridge'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '桥梁-零件工序定义',
    sort: [],
    hasPagination: false,
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi, get: getMachinePart }
  },
  tableRef
)

const { maxHeight } = useMaxHeight()

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.classifyLinkName = v.classifyLinks?.map((v) => `【${v.name}】`).join('')
    v.specPrefix = v.pecPrefixList?.map((v) => `【${v}】`).join('')
    v.processSequence = v.productProcessLinkList?.map((v) => `【${v.name}】`).join('→')
    v.processSequenceIds = v.productProcessLinkList?.map((v) => v.processId)
    return v
  })
}
</script>
