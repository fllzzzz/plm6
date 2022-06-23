<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <common-button size="mini" @click="showType='add';formVisible=true;" icon="el-icon-plus" type="primary" v-permission="permission.add">新增</common-button>
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      class="upload-table"
      :data="list"
      v-loading="tableLoading"
      :empty-text="'暂无数据'"
      :max-height="maxHeight"
      :row-class-name="handleRowClassName"
      :cell-class-name="cellClassName"
      :stripe="false"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column key="productionLineType" prop="productionLineType" align="center" :show-overflow-tooltip="true" label="生产线">
        <template v-slot="scope">
          <span>{{ scope.row.productionLineType ? artifactProductLineEnum.VL[scope.row.productionLineType] : '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column key="mainClassificationName" prop="mainClassificationName" align="center" :show-overflow-tooltip="true" label="构件类型">
        <template v-slot="scope">
          <span>{{ scope.row.mainClassificationName }}</span>
        </template>
      </el-table-column>
      <el-table-column key="classificationName" prop="classificationName" align="center" :show-overflow-tooltip="true" label="子分类" min-width="120">
        <template v-slot="scope">
          <template v-if="scope.row.productionLineType === artifactProductLineEnum.INTELLECT.V">
            <div v-for="(item,index) in scope.row.structureClassificationList" :key="item.id" :class="index === scope.row.structureClassificationList.length-1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'" :style="`height:${item.styleHeight};line-height:${item.lineHeight}`">
                <span style="margin-left:5px;">{{ item.classificationName }}</span>
                <template  v-if="item.parentType === intellectParentType.BRIDGE.V">
                  <span v-if="item.minLength && item.maxLength">（{{item.minLength}}mm {{ item.boolContainsMin ? '≤' : '&lt;' }} 长度 {{ item.boolContainsMax ? '≤' : '&lt;' }} {{ item.maxLength}}mm）</span>
                  <span v-else-if="item.minLength">（{{ item.boolContainsMin ? '≥' : '&gt;' }}{{ item.minLength }}mm）</span>
                  <span v-else-if="item.maxLength">（{{ item.boolContainsMax ? '≤' : '&lt;' }}{{ item.maxLength}}mm）</span>
                </template>
            </div>
          </template>
          <span v-else>-</span>
        </template>
      </el-table-column>
      <el-table-column key="specPrefixList" prop="specPrefixList" label="构件规格前缀" align="center">
        <template v-slot="scope">
          <div v-for="(item,index) in scope.row.structureClassificationList" :key="item.id">
            <template v-if="item.specPrefixList && item.specPrefixList.length > 0">
              <div v-for="(k,i) in item.specPrefixList" :key="k.id">
                <div :class="i === item.specPrefixList.length - 1 ? (index === scope.row.structureClassificationList.length-1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top') : 'sandwich-cell-top'">
                  {{ k.specPrefix }}
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom">-</div>
          </div>
        </template>
      </el-table-column>
      <!-- <el-table-column key="specPrefixList" prop="specPrefixList" label="是否匹配部件" align="center">
        <template v-slot="scope">
          <div v-for="(item,index) in scope.row.structureClassificationList" :key="item.id">
            <template v-if="item.specPrefixList && item.specPrefixList.length > 0">
              <div v-for="(k,i) in item.specPrefixList" :key="k.id">
                <div :class="i === item.specPrefixList.length - 1 ? (index === scope.row.structureClassificationList.length-1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top') : 'sandwich-cell-top'">
                  {{ k.boolUseAssemble ? '√' : '-' }}
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom">-</div>
          </div>
        </template>
      </el-table-column> -->
      <el-table-column key="definitionWord" prop="definitionWord" align="center" :show-overflow-tooltip="true" label="定义代码">
        <template v-slot="scope">
          <div v-for="(item,index) in scope.row.structureClassificationList" :key="item.id" :class="index === scope.row.structureClassificationList.length-1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'" :style="`height:${item.styleHeight};line-height:${item.lineHeight}`">
            {{ item.definitionWord || '-' }}
          </div>
        </template>
      </el-table-column>
      <el-table-column
        key="sort"
        prop="sort"
        :show-overflow-tooltip="true"
        label="排序"
        width="80"
        align="center"
      >
        <template v-slot="scope">
          <div v-for="(item,index) in scope.row.structureClassificationList" :key="item.id" :class="index === scope.row.structureClassificationList.length-1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'" :style="`height:${item.styleHeight};line-height:${item.lineHeight}`">
            <span>{{ item.sort }}</span>
          </div>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.del, ...permission.edit])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <div v-for="(item,index) in scope.row.structureClassificationList" :key="item.id" :class="index === scope.row.structureClassificationList.length-1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'" :style="`height:${item.styleHeight};line-height:${item.lineHeight}`">
            <common-button size="mini" @click="openForm(item,'edit')" icon="el-icon-edit" type="primary" v-permission="permission.edit"/>
            <el-popconfirm
                confirm-button-text="确定"
                cancel-button-text="取消"
                icon-color="red"
                title="确定删除吗?"
                @confirm="handleDelete(item,k)"
                v-if="checkPermission(permission.del)"
              >
                <template #reference>
                  <common-button size="small" class="el-icon-delete" type="danger"/>
                </template>
              </el-popconfirm>
          </div>
        </template>
      </el-table-column>
    </common-table>
    <mForm v-model="formVisible" :detailInfo="detailInfo" :showType="showType" @success="fetchList"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/config/system-config/artifact-config'
import { ref } from 'vue'
import { ElNotification } from 'element-plus'

import { artifactConfigPM as permission } from '@/page-permission/config'
import { artifactProductLineEnum, intellectParentType } from '@enum-ms/mes'

import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import mForm from './module/form'

const tableRef = ref()
const list = ref([])
const tableLoading = ref(false)
const formVisible = ref(false)
const detailInfo = ref({})
const showType = ref('add')

const { maxHeight } = useMaxHeight({
  wrapperBox: '.artifact-config',
  paginate: true,
  extraHeight: 40
})

function handleRowClassName({ row, rowIndex }) {
  if (row.productionLineType === artifactProductLineEnum.INTELLECT.V) {
    return 'abnormal-row'
  } else {
    return 'blue-row'
  }
}
function cellClassName({ row, rowIndex }) {
  if (row.productionLineType === artifactProductLineEnum.INTELLECT.V) {
    return 'abnormal-row'
  } else {
    return 'blue-row'
  }
}

async function handleDelete(row, index) {
  try {
    await crudApi.del([row.id])
    ElNotification.success(`删除成功`)
    fetchList()
  } catch (e) {
    console.log(`删除`, e)
  }
}

function openForm(item, type) {
  showType.value = type
  detailInfo.value = item
  formVisible.value = true
}

fetchList()

async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [] } = await crudApi.get()
    content.forEach(v => {
      if (v.structureClassificationList?.length) {
        v.structureClassificationList.map((k, index) => {
          k.styleHeight = k.specPrefixList.length ? k.specPrefixList.length * 40 + 'px' : '40px'
          k.lineHeight = k.specPrefixList.length ? k.specPrefixList.length * 30 + 'px' : '30px'
        })
        v.mainClassificationName = v.productionLineType === artifactProductLineEnum.INTELLECT.V ? (v.parentType ? intellectParentType.VL[v.parentType] : '-') : v.structureClassificationList[0].classificationName
      }
    })
    _list = content
  } catch (error) {
    console.log('获取构件类型配置失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #f5e4e4;
}
::v-deep(.blue-row) {
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
  padding: 4px 0;
}
.float-ele{
  float:left;
}
</style>
