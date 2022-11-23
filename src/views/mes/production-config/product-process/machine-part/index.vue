<template>
  <div class="app-container">
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
      <el-table-column prop="name" align="center" :show-overflow-tooltip="true" label="零件类型" min-width="200"></el-table-column>
      <el-table-column prop="specSequence" :show-overflow-tooltip="true" label="规格前缀索引" min-width="300"></el-table-column>
      <el-table-column prop="productProcessLinkList" label="工序" min-width="200">
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
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.edit]" label="操作" width="100px" align="center" fixed="right">
        <template v-slot="scope">
          <udOperation :data="scope.row" :disabledEdit="scope.row.basicClass !== 1" :showDel="false" />
        </template>
      </el-table-column>
    </common-table>
    <mForm />
  </div>
</template>

<script setup>
import crudApi, { getMachinePart } from '@/api/mes/production-config/product-process'
import { ref } from 'vue'
import { configProductProcessMachinePartPM as permission } from '@/page-permission/config'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mForm from './module/form'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, CRUD } = useCRUD(
  {
    title: '零件工序定义',
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
    v.specSequence = v.links?.map((v) => `${v.keyword}【${v.specIndex ? v.specIndex : '全部'}】`).join('、')
    v.processSequence = v.productProcessLinkList?.map((v) => `【${v.name}】`).join('→')
    v.processSequenceIds = v.productProcessLinkList?.map((v) => v.processId)
    return v
  })
}
</script>
