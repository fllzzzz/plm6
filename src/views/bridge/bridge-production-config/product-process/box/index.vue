<template>
  <div class="app-container">
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :stripe="false"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="name" align="center" :show-overflow-tooltip="true" label="分段类型">
        <template #default="{ row }">
          <span>{{ row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="specPrefixSequence" align="center" :show-overflow-tooltip="true" label="分段前缀"> </el-table-column>
      <el-table-column prop="productProcessLinkList" label="工序" min-width="200">
        <template #default="{ row: { sourceRow: row } }">
          <el-tooltip :content="`${row.processSequence}`" placement="top-start">
            <div style="display: flex; align-items: center; flex-wrap: wrap; white-space: nowrap">
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
          <udOperation :data="scope.row" :showDel="false" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script setup>
import crudApi, { getBox } from '@/api/bridge/production-config/product-process'
import { ref } from 'vue'
import { configProductProcessAssemblePM as permission } from '@/page-permission/config'
// import { isNotBlank, deepClone } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
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
    title: '分段工序',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi, get: getBox }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content = data.content.map((o) => {
    o.processSequenceObj = {}
    o.specPrefixSequence = o.specPrefixList?.map((v) => `【${v.specPrefix}】`).join('') || ''
    o.processSequence = o.productProcessLinkList?.map((v) => `【${v.name}】${v.nodeTime ? '→ ' + v.nodeTime + ' ' : ''}`).join('→')
    o.processSequenceIds = o.productProcessLinkList?.map((v) => {
      o.processSequenceObj[v.processId] = v.nodeTime
      return v.processId
    })
    return o
  })
}
</script>
