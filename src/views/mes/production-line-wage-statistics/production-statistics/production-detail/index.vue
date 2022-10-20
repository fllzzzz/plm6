<template>
  <common-table
    ref="tableRef"
    :data="workshopList"
    :empty-text="'暂无数据'"
    :max-height="maxHeight"
    row-key="id"
    style="width: 100%"
    class="upload-table"
  >
    <el-table-column align="center" key="workshop" prop="workshop" :show-overflow-tooltip="true" label="车间/产线">
      <template v-slot="scope">
        <div>{{ scope.row.workshop }}>{{ scope.row.productionLine }}</div>
      </template>
    </el-table-column>
    <el-table-column align="center" key="team" prop="team" :show-overflow-tooltip="true" label="班组">
      <template v-slot="scope">
        <template v-if="scope.row.productionDetail.length > 0">
          <div v-for="(item, i) in scope.row.productionDetail" :key="item">
            <div :class="i === scope.row.productionDetail.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'">
              {{ item.team }}
            </div>
          </div>
        </template>
        <div v-else class="sandwich-cell-bottom"></div>
      </template>
    </el-table-column>
    <el-table-column align="center" key="production" prop="production" :show-overflow-tooltip="true" label="产量（件/吨）">
      <template v-slot="scope">
        <template v-if="scope.row.productionDetail.length > 0">
          <div v-for="(item, i) in scope.row.productionDetail" :key="item">
            <div :class="i === scope.row.productionDetail.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'">
              {{ item.productionQuantity }}/{{ item.productionWeight }}
            </div>
          </div>
        </template>
        <div v-else class="sandwich-cell-bottom"></div>
      </template>
    </el-table-column>
    <el-table-column align="center" key="wage" prop="wage" :show-overflow-tooltip="true" label="工资（元）">
      <template v-slot="scope">
        <template v-if="scope.row.productionDetail.length > 0">
          <div v-for="(item, i) in scope.row.productionDetail" :key="item">
            <div :class="i === scope.row.productionDetail.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'">
              {{ item.wage }}
            </div>
          </div>
        </template>
        <div v-else class="sandwich-cell-bottom"></div>
      </template>
    </el-table-column>
    <el-table-column align="center" key="productionProportion" prop="productionProportion" :show-overflow-tooltip="true" label="产量占比">
      <template v-slot="scope">
        <template v-if="scope.row.productionDetail.length > 0">
          <div v-for="(item, i) in scope.row.productionDetail" :key="item">
            <div :class="i === scope.row.productionDetail.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'">
              <el-progress
                :text-inside="true"
                stroke-linecap="square"
                :stroke-width="24"
                :percentage="item.productionProportion"
                status="success"
              />
            </div>
          </div>
        </template>
        <div v-else class="sandwich-cell-bottom"></div>
      </template>
    </el-table-column>
    <el-table-column align="center" :show-overflow-tooltip="true" label="操作">
      <template v-slot="scope">
        <template v-if="scope.row.productionDetail.length > 0">
          <div v-for="(item, i) in scope.row.productionDetail" :key="item">
            <div :class="i === scope.row.productionDetail.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'">
              <common-button type="primary" size="mini" @click="views(item)">查看</common-button>
            </div>
          </div>
        </template>
        <div v-else class="sandwich-cell-bottom"></div>
      </template>
    </el-table-column>
  </common-table>
  <artifact v-model="visible" :production-data="productionData" />
  <!-- <part v-model="visible"  />
  <perforate v-model="visible" /> -->
</template>

<script setup>
import { ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import artifact from '../module/artifact.vue'
// import part from '../module/part.vue'
// import perforate from '../module/perforate.vue'

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
const visible = ref(false)
const productionData = ref({})
const workshopList = [
  {
    workshop: '一车间',
    productionLine: '一线',
    productionDetail: [
      { team: '一组（超级管理员）', productionQuantity: 60, productionWeight: 1000, wage: 100, productionProportion: 60, process: '组立' },
      {
        team: '二组（管理员）',
        productionQuantity: 60,
        productionWeight: 1000,
        wage: 100,
        productionProportion: 45,
        process: '下料'
      }
    ]
  },
  {
    workshop: '一车间',
    productionLine: '二线',
    productionDetail: [
      {
        team: '三组（超级管理员）',
        productionQuantity: 60,
        productionWeight: 1000,
        wage: 100,
        productionProportion: 35,
        process: '钻孔'
      },
      {
        team: '四组（超级管理员）',
        productionQuantity: 60,
        productionWeight: 1000,
        wage: 100,
        productionProportion: 50,
        process: '涂装'
      }
    ]
  }
]

function views(item) {
  console.log(item, 'item')
  visible.value = true
  productionData.value = item
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1) {
    .cell {
      opacity: 0;
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
.sandwich-cell-top,
.sandwich-cell-bottom {
  padding: 5px;
  height: 40px;
  line-height: 30px;
  box-sizing: border-box;
  overflow: hidden;
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 2px;
  }
}
.sandwich-cell-top {
  border-bottom: 1px solid #dfe6ec;
}
.upload-table {
  ::v-deep(.cell) {
    padding-left: 0;
    padding-right: 0;
  }
  ::v-deep(thead.is-group th) {
    background: #fff;
  }
}
::v-deep(.el-table--small .el-table__cell) {
  padding: 3px 0;
}
</style>
