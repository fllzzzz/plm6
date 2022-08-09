<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <common-table
      ref="tableRef"
      v-if="tableRefresh"
      v-loading="crud.loading"
      border
      :data="list"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :span-method="spanMethod"
      :highlight-current-row="false"
      @selection-change="crud.selectionChangeHandler"
      row-key="id"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column key="index" type="index" label="序号" align="center" width="60" />
      <el-table-column v-if="columns.visible('level1')" prop="level1" label="一级" align="left" min-width="160px">
        <template #header>
          <el-tooltip effect="light" :content="`一级科目（名称-编号）`" placement="top">
            <div>
              <span>一级科目</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <div class="classifyName">
            <span>{{ `${scope.row.fullName[0]}-${scope.row.fullSerialNumber[0]}` }}</span>
          </div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('level2')" prop="level2" label="二级" align="left" min-width="160px">
        <template #header>
          <el-tooltip effect="light" :content="`二级科目（名称-编号）`" placement="top">
            <div>
              <span>二级科目</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <div class="classifyName">
            <template v-if="scope.row.fullName.length > 1">
              <span>{{ `${scope.row.fullName[1]}-${scope.row.fullSerialNumber[1]}` }}</span>
            </template>
          </div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('level3')" prop="level3" label="三级" align="left" min-width="160px">
        <template #header>
          <el-tooltip effect="light" :content="`三级科目（名称-编号）`" placement="top">
            <div style="margin: 0 10px">
              <span>三级科目</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <div class="classifyName">
            <template v-if="scope.row.fullName.length > 2">
              <span>{{ `${scope.row.fullName[2]}-${scope.row.fullSerialNumber[2]}` }}</span>
            </template>
          </div>
        </template>
      </el-table-column>
      <!--删除-->
      <el-table-column v-if="checkPermission([...permission.del])" label="操作" width="90px" align="center" fixed="right">
        <template #default="{ row }">
          <udOperation :data="row" :showEdit="false" />
        </template>
      </el-table-column>
    </common-table>
    <m-form :disabled-ids="disabledIds" />
  </div>
</template>

<script setup>
import crudApi from '@/api/config/wms/inspection-raw-material'
import { configWmsInspectionRawMaterialPM as permission } from '@/page-permission/config'

import { nextTick, onUnmounted, ref, computed } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import { isNotBlank } from '@data-type'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'

const optShow = {
  add: true,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const tableRefresh = ref(true)
const disabledIds = ref([])
const { maxHeight } = useMaxHeight()

const { crud, columns, CRUD } = useCRUD(
  {
    title: '入库质检物料',
    sort: ['sort.asc', 'id.desc'],
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow },
    hasPagination: false
  },
  tableRef
)

const list = computed(() => {
  let list = crud.data
  if (crud.query) {
    const _name = crud.query?.name
    const _code = crud.query?.code
    const _basicClass = crud.query?.basicClass
    console.log(crud.query)
    list = list.filter((v) => {
      let flag = true
      if (_name && !v.fullName.some((v) => v.indexOf(_name) > -1)) {
        flag = false
      }
      if (_code && !v.fullSerialNumber.some((v) => v.indexOf(_code) > -1)) {
        flag = false
      }
      if (_basicClass && !(v.basicClass & _basicClass)) {
        flag = false
      }
      return flag
    })
  }
  return mergeCells(list)
})

onUnmounted(() => {
  // TODO:在离开该页面时重新拉取科目列表以获取最新的数据
  // store.dispatch('interfaceCache/fetchSubjectSimple')
})

CRUD.HOOK.handleRefresh = (crud, res) => {
  disabledIds.value = []
  getDisabledIds(res.data.content)
  res.data.content = classificationTreeFormat(res.data.content)
}

function getDisabledIds(tree) {
  tree.forEach((v) => {
    if (isNotBlank(v.children)) {
      if (v.classifyQuantity === v.children.length) {
        disabledIds.value.push(v.id)
      }
      getDisabledIds(v.children)
    } else {
      disabledIds.value.push(v.id)
    }
  })
}

function classificationTreeFormat(tree, deep = 1, extendsData = { fullId: [], fullName: [], fullCode: [], fullSerialNumber: [] }) {
  // 表格展示的数据（所有末级科目）
  const tableData = []

  // 遍历科目数
  tree.forEach((node, index) => {
    // 因为再单位设置页面用到了下面的一些参数，而单位设置页面不需要继承的信息，因此直接设置在node中
    node.fullId = [...extendsData.fullId, node.id] // id-全路径
    node.fullName = [...extendsData.fullName, node.name] // name-全路径
    node.fullCode = [...extendsData.fullCode, node.code] // code（编码）-全路径
    node.fullSerialNumber = [...extendsData.fullSerialNumber, extendsData.fullCode.join('') + node.code] // 科目完整编码-全路径
    // 设置节点信息
    const leafNode = {}
    Object.assign(leafNode, JSON.parse(JSON.stringify(extendsData)), node)

    // 设置当前层信息
    const nodeChildren = leafNode.children
    if (isNotBlank(nodeChildren)) {
      const e = {
        fullId: leafNode.fullId, // id-全路径
        fullName: leafNode.fullName, // 名称-全路径
        fullCode: leafNode.fullCode, // 编码-路径
        fullSerialNumber: leafNode.fullSerialNumber, // 编码-路径
        attribute: leafNode.attribute // 类型
      }
      const leafNodes = classificationTreeFormat(nodeChildren, deep + 1, e)
      tableData.push.apply(tableData, leafNodes)
    } else {
      tableData.push(leafNode)
    }
    // 删除 children
    delete node.children
  })

  return tableData
}

// 合并单元格
function mergeCells(list) {
  if (list.length === 0) return list
  const row = [[], []]
  const id = [-1, -1]
  list.forEach((v) => {
    for (let i = 0; i < row.length; i++) {
      const newId = v.fullId[i]
      const oldId = id.length > i ? id[i] : undefined
      if (newId === oldId && isNotBlank(oldId)) {
        row[i][row[i].length - 1]++
      } else {
        row[i].push(1)
        id[i] = newId
      }
    }
  })
  row[0].reduce((total, cur) => {
    list[total].name1_rowSpan = cur
    total += cur
    return total
  }, 0)
  row[1].reduce((total, cur) => {
    list[total].name2_rowSpan = cur
    total += cur
    return total
  }, 0)
  setTimeout(() => {
    tableRefresh.value = false
    nextTick(() => {
      tableRefresh.value = true
    })
  }, 1000)
  return list
}

/**
 * columnIndex === 2 时 colspan 的 取值（即二级科目列）
 * index = 0 则 colspan = 0 只存在一级科目，当前列隐藏（被一级占用）
 * index = 1 则 colspan = 2 二级科目，跨两列（占用三级科目列）
 * index = 2 则 colspan = 1 存在三级科目，当前列不做任何操作
 */
const col2Arr = [0, 2, 1]
function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 2) {
    return {
      rowspan: row.name1_rowSpan || 0,
      colspan: row.fullName.length === 1 ? 3 : 1
    }
  }
  if (columnIndex === 3) {
    return {
      rowspan: row.name2_rowSpan || 0,
      colspan: col2Arr[row.fullName.length - 1]
    }
  }
  if (columnIndex === 4) {
    return {
      rowspan: 1,
      colspan: row.fullName.length < 3 ? 0 : 1
    }
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.el-table) {
  th,
  td {
    padding: 0;
  }
  .el-tooltip {
    line-height: 40px;
  }
  .cell {
    line-height: 40px;
  }
  th:first-child .cell,
  td:first-child .cell {
    padding: 0;
  }
  .el-table__body .el-input__inner,
  .el-table__body .el-textarea__inner {
    border-radius: 0;
  }
}
</style>
