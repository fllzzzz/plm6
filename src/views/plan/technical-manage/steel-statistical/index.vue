<template>
  <div class="app-container">
    <template v-if="globalProject">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :projectId="globalProjectId"/>
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        style="width: 100%"
        show-summary
        :summary-method="getSummaries"
        return-source-data
        :showEmptySymbol="false"
      >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="钢材种类" width="140px">
        <template v-slot="scope">
          <span>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('thickness')" key="thickness" prop="thickness" :show-overflow-tooltip="true" label="厚度/规格" min-width="160px">
        <template v-slot="scope">
          <span>{{ scope.row.thickness }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('material')" key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="160px">
        <template v-slot="scope">
          <span>{{ scope.row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('listMete')" key="listMete" prop="listMete" :show-overflow-tooltip="true" label="清单量(kg)" min-width="160px">
        <template v-slot="scope">
          <span>{{ scope.row.listMete }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('stockMete')" key="stockMete" prop="stockMete" :show-overflow-tooltip="true" label="当前库存(kg)" min-width="160px">
        <template v-slot="scope">
          <span>{{ scope.row.stockMete }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('diff')" key="diff" prop="diff" :show-overflow-tooltip="true" label="差异" min-width="160px">
        <template v-slot="scope">
          <span :class="scope.row.diffClass">{{ scope.row.diff }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('diffRate')" key="diffRate" prop="diffRate" :show-overflow-tooltip="true" label="差异率" min-width="160px">
        <template v-slot="scope">
          <span :class="scope.row.diffRateClass">{{ scope.row.diffRate }}</span>
        </template>
      </el-table-column>
    </common-table>
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/steel-statistical'
import { ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { steelStatisticalPM as permission } from '@/page-permission/plan'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '钢材使用用量对比',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId'],
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.steel-statistical',
  paginate: true,
  extraHeight: 40
})

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    v.diffClass = v.diff && v.diff >= 0 ? 'green' : 'red'
    v.diffRateClass = v.diffRate && v.diffRate >= 0 ? 'green' : 'red'
    v.diffRate = (v.diffRate * 100).toFixed(2) + '%'
    return v
  })
}

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  const property = ['listMete', 'stockMete', 'diff', 'diffRate']
  const sumData = {}
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (!property.includes(column.property)) return
    const values = data.map(item => Number(item[column.property]))
    if (!values.every(value => isNaN(value))) {
      sums[index] = values.reduce((prev, curr) => {
        const value = Number(curr)
        if (!isNaN(value)) {
          return prev + curr
        } else {
          return prev
        }
      }, 0)
      sumData[column.property] = sums[index]
    }
    if (column.property === 'diffRate') {
      sums[index] = Number(sumData.diff) && Number(sumData.listMete) ? ((sumData.diff / sumData.listMete) * 100).toFixed(2) + '%' : '-'
    } else {
      sums[index] = Number(sums[index]).toFixed(2)
    }
  })
  return sums
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
.red{
  color: red;
}
.green{
  color: rgb(14, 192, 124);
}
</style>
