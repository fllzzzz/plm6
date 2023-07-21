<template>
  <div class="app-container">
    <div class="flex-r">
      <el-card body-style="padding: 10px" class="gas-tree">
        <template #header>
          <div>气体分类</div>
        </template>
        <el-tree
          ref="gasTreeRef"
          v-loading="crud.loading"
          :style="{ maxHeight: maxHeight - 20 + 'px' }"
          :data="gasTree"
          :props="defaultProps"
          :expand-on-click-node="false"
          node-key="id"
          highlight-current
          default-expand-all
          @node-click="nodeClick"
        />
      </el-card>
      <div style="flex: 1; min-width: 1px">
        <mHeader :row-detail="rowDetail" />
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          :empty-text="crud.emptyText"
          :max-height="maxHeight"
          row-key="id"
          show-summary
          :data-format="columnsDataFormat"
          :summary-method="getSummaries"
        >
          <el-table-column v-if="columns.visible('date')" prop="date" key="date" :label="`${crud.query.year}年`" align="center" />
          <el-table-column
            v-if="columns.visible('classifyName')"
            align="center"
            key="classifyName"
            prop="classifyName"
            :show-overflow-tooltip="true"
            label="气体种类"
          />
          <el-table-column
            v-if="columns.visible('accountingUnit')"
            align="center"
            key="accountingUnit"
            prop="accountingUnit"
            :show-overflow-tooltip="true"
            label="核算单位"
          />
          <el-table-column
            v-if="columns.visible('usedMete')"
            align="center"
            key="usedMete"
            prop="usedMete"
            :show-overflow-tooltip="true"
            label="使用量"
          />
          <el-table-column
            v-if="columns.visible('totalAmount')"
            align="center"
            key="totalAmount"
            prop="totalAmount"
            :show-overflow-tooltip="true"
            label="总额"
          />
          <el-table-column
            v-if="columns.visible('avgUnitPrice')"
            align="center"
            key="avgUnitPrice"
            prop="avgUnitPrice"
            :show-overflow-tooltip="true"
            label="平均单价"
          />
          <el-table-column v-if="checkPermission([...permission.edit, ...permission.del])" align="center" label="操作" width="140px">
            <template #default="{ row }">
              <el-tag v-if="row.isAmortization" size="medium" type="success" effect="plain"> 已摊销 </el-tag>
              <udOperation v-else-if="row.isEdit" :data="row" />
              <udOperation v-else :data="row" :disabled-edit="true" :disabled-del="true" />
            </template>
          </el-table-column>
        </common-table>
      </div>
    </div>
    <!-- 表单 -->
    <m-form :gas-tree="gasTree" :row-detail="rowDetail" :lastGasKV="lastGasKV" />
  </div>
</template>

<script setup>
import { ref, nextTick } from 'vue'
import crudApi, { subjectTree } from '@/api/contract/expense-entry/gas-cost'

import { gasCostPM as permission } from '@/page-permission/contract'
import { tableSummary } from '@/utils/el-extra'
import { matClsEnum } from '@enum-ms/classification'
import { setEmptyArr2Undefined, setLevelName } from '@/utils/data-type/tree'
import { toThousand } from '@/utils/data-type/number'
import moment from 'moment'
import checkPermission from '@/utils/system/check-permission'
import { DP } from '@/settings/config'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header.vue'
import mForm from './module/form.vue'

const tableRef = ref()
const gasTreeRef = ref()
const gasTree = ref([])
const rowDetail = ref({})
const lastGasKV = ref({}) // 末级气体{id: data}

const defaultProps = ref({
  children: 'children',
  label: 'name'
})

const columnsDataFormat = ref([
  ['avgUnitPrice', 'to-thousand'],
  ['usedMete', 'to-thousand']
])

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const { crud, CRUD, columns } = useCRUD(
  {
    title: '气体统计',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false,
    hasPagination: false
  },
  tableRef
)

gasListGet()

async function gasListGet() {
  try {
    gasTree.value = (await subjectTree({ basicClassEnum: matClsEnum.GAS.V })) || []
    setEmptyArr2Undefined(gasTree.value)
    setLevelName(gasTree.value)
    setLastGas(gasTree.value)
    nextTick(() => {
      if (gasTree.value.length) {
        selectLast(gasTree.value)
      } else {
        crud.toQuery()
      }
    })
  } catch (e) {
    console.log('获取气体类型失败', e)
  }
}

// 设置末级气体
function setLastGas(tree = []) {
  tree?.forEach((row) => {
    if (row?.children?.length) {
      setLastGas(row.children)
    } else {
      lastGasKV.value[row.id] = row
    }
  })
}

// 默认选中第一个末级
function selectLast(tree = []) {
  if (tree.length) {
    if (tree[0].children?.length) {
      selectLast(tree[0].children)
    } else {
      nodeClick(tree[0])
    }
  }
}

// el-tree 左键点击
function nodeClick(row = {}) {
  // 取消选中
  if (row.id && row.id === rowDetail.value.id) {
    row = {}
  }
  rowDetail.value = row
  gasTreeRef.value.setCurrentKey(row.id)
  crud.query.classifyId = row.id
  crud.toQuery()
}

// 合计
function getSummaries(param) {
  const data = tableSummary(param, {
    props: ['usedMete', 'totalAmount']
  })
  if (data[3] && data[4]) {
    data[5] = toThousand(data[4] / data[3], DP.YUAN)
  }
  if (data[3]) {
    data[3] = toThousand(data[3])
  }
  if (data[4]) {
    data[4] = toThousand(data[4], DP.YUAN)
  }
  return data
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  const length = data.length
  data.content = data.map((v, i) => {
    // 时间范围
    let _startDate = moment(v.startDate).format('YYYY')
    let _endDate = moment(v.endDate).format('YYYY')
    if (_startDate !== crud.query.year || _endDate !== crud.query.year) {
      _startDate = moment(v.startDate).format('YYYY-MM-DD')
      _endDate = moment(v.endDate).format('YYYY-MM-DD')
    } else {
      _startDate = moment(v.startDate).format('MM-DD')
      _endDate = moment(v.endDate).format('MM-DD')
    }
    v.date = `${_startDate} ~ ${_endDate}`
    // 最后一条记录才能编辑并且不能为已摊销状态
    v.isEdit = i + 1 === length && !v.isAmortization
    return v
  })
}
const { maxHeight } = useMaxHeight()
</script>
<style lang="scss" scoped>
::v-deep(.gas-tree) {
  width: 240px;
  margin-right: 20px;
  .el-card__header {
    padding: 10px 15px;
    background: #e6e1e1;
  }
  .el-tree {
    overflow-y: auto;
  }
}
</style>
