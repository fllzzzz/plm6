<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <common-button size="mini" @click="showType='add';formVisible=true;" icon="el-icon-plus" type="primary" v-permission="permission.add" class="filter-item">新增</common-button>
      <common-radio-button
        v-model="productionLineTypeEnum"
        :options="artifactProductLineEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        @change="lineTypeChange"
        class="filter-item"
      />
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
      :span-method="spanMethod"
      :stripe="false"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column key="productionLineTypeEnum" prop="productionLineTypeEnum" align="center" :show-overflow-tooltip="true" label="生产线" width="80">
        <template v-slot="scope">
          <span>{{ scope.row.productionLineTypeEnum ? artifactProductLineEnum.VL[scope.row.productionLineTypeEnum] : '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column key="mainClassificationName" prop="mainClassificationName" align="center" :show-overflow-tooltip="true" label="类型">
        <template v-slot="scope">
          <span>{{ scope.row.mainClassificationName }}</span>
        </template>
      </el-table-column>
      <el-table-column key="classificationName" prop="classificationName" align="center" :show-overflow-tooltip="true" label="子分类" min-width="120" v-if="productionLineTypeEnum!==artifactProductLineEnum.TRADITION.V">
        <template v-slot="scope">
          <template v-if="scope.row.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V">
            <div v-for="(item,index) in scope.row.structureClassificationList" :key="item.id" :class="index === scope.row.structureClassificationList.length-1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'">
                <span style="margin-left:5px;">{{ item.classificationName }}</span>
                <!-- <template  v-if="item.parentType === intellectParentType.BRIDGE.V">
                  <div v-if="item.minLength && item.maxLength">（{{item.minLength}}mm &lt; 长度 {{ '≤' }} {{ item.maxLength}}mm）</div>
                  <div v-else-if="item.minLength">（&gt;{{ item.minLength }}mm）</div>
                  <div v-else-if="item.maxLength">（{{ '≤' }}{{ item.maxLength}}mm）</div>
                </template> -->
            </div>
          </template>
          <span v-else>-</span>
        </template>
      </el-table-column>
      <el-table-column key="artifactType" prop="artifactType" align="center" :show-overflow-tooltip="true" label="构件类型" v-if="productionLineTypeEnum!==artifactProductLineEnum.INTELLECT.V">
        <template v-slot="scope">
          <span>{{ scope.row.artifactType? artifactTypeEnum.VL[scope.row.artifactType] : '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column key="specPrefixList" prop="specPrefixList" label="构件规格前缀" align="center" min-width="120" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div v-for="(item,index) in scope.row.structureClassificationList" :key="item.id" :class="index === scope.row.structureClassificationList.length-1 ? 'sandwich-cell-bottom div-ellipsis' : 'sandwich-cell-top div-ellipsis'">
            <template v-if="item.specPrefixList && item.specPrefixList.length > 0">
              <span v-for="k in item.specPrefixList" :key="k.id">{{`【${k.specPrefix}】`}}</span>
            </template>
          </div>
        </template>
      </el-table-column>
       <el-table-column key="serialNumberPrefixList" prop="serialNumberPrefixList" label="编号前缀" align="center" min-width="120" :show-overflow-tooltip="true" v-if="productionLineTypeEnum!==artifactProductLineEnum.INTELLECT.V">
        <template v-slot="scope">
          <div v-for="(item,index) in scope.row.structureClassificationList" :key="item.id" :class="index === scope.row.structureClassificationList.length-1 ? 'sandwich-cell-bottom div-ellipsis' : 'sandwich-cell-top div-ellipsis'">
            <template v-if="scope.row.artifactType===artifactTypeEnum.SMALL.V && item.serialNumberPrefixList && item.serialNumberPrefixList.length > 0">
              <span v-for="k in item.serialNumberPrefixList" :key="k.id">{{`【${k.serialNumberPrefix}】`}}</span>
            </template>
            <span v-else>-</span>
          </div>
        </template>
      </el-table-column>
      <el-table-column key="codingType" prop="codingType" align="center" :show-overflow-tooltip="true" label="打码方式" width="100" v-if="productionLineTypeEnum!==artifactProductLineEnum.INTELLECT.V">
        <template v-slot="scope">
          <span>{{ scope.row.codingType && scope.row.productionLineTypeEnum===artifactProductLineEnum.TRADITION.V? codingTypeEnum.VL[scope.row.codingType]:'-' }}</span>
        </template>
      </el-table-column>
      <el-table-column key="definitionWord" prop="definitionWord" align="center" :show-overflow-tooltip="true" label="定义代码" v-if="productionLineTypeEnum!==artifactProductLineEnum.TRADITION.V">
        <template v-slot="scope">
          <div v-for="(item,index) in scope.row.structureClassificationList" :key="item.id" :class="index === scope.row.structureClassificationList.length-1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'">
            {{ item.definitionWord || '-' }}
          </div>
        </template>
      </el-table-column>
      <el-table-column
        key="sort"
        prop="sort"
        :show-overflow-tooltip="true"
        label="排序"
        width="60"
        align="center"
      >
        <template v-slot="scope">
          <div v-for="(item,index) in scope.row.structureClassificationList" :key="item.id" :class="index === scope.row.structureClassificationList.length-1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'">
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
          <div v-for="(item,index) in scope.row.structureClassificationList" :key="item.id" :class="index === scope.row.structureClassificationList.length-1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'">
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
import { artifactProductLineEnum, intellectParentType, artifactTypeEnum, codingTypeEnum } from '@enum-ms/mes'

import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import mForm from './module/form'

const tableRef = ref()
const list = ref([])
const tableLoading = ref(false)
const formVisible = ref(false)
const detailInfo = ref({})
const showType = ref('add')
const productionLineTypeEnum = ref()
const traditionArr = ref([])
const intellectArr = ref([])

const { maxHeight } = useMaxHeight({
  wrapperBox: '.artifact-config',
  paginate: true,
  extraHeight: 40
})

function handleRowClassName({ row, rowIndex }) {
  if (row.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V) {
    return 'abnormal-row'
  } else {
    return 'blue-row'
  }
}
function cellClassName({ row, rowIndex }) {
  if (row.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V) {
    return 'abnormal-row'
  } else {
    return 'blue-row'
  }
}

// 合并单元格
function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 1) {
    return {
      rowspan: row.rowSpan || 0,
      colspan: 1
    }
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

function lineTypeChange(val) {
  if (val) {
    list.value = val === artifactProductLineEnum.INTELLECT.V ? intellectArr.value : traditionArr.value
  } else {
    list.value = [].concat(traditionArr.value, intellectArr.value)
  }
}

fetchList()

async function fetchList() {
  let _list = []
  traditionArr.value = []
  intellectArr.value = []
  tableLoading.value = true
  try {
    const { content = [] } = await crudApi.get()
    content.forEach(v => {
      if (v.structureClassificationList?.length) {
        v.mainClassificationName = v.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V ? (v.parentType ? intellectParentType.VL[v.parentType] : '-') : v.structureClassificationList[0].classificationName
        v.artifactType = v.structureClassificationList[0].artifactType
        v.codingType = v.structureClassificationList[0].codingType
      }
    })
    traditionArr.value = content.filter(v => v.productionLineTypeEnum === artifactProductLineEnum.TRADITION.V)
    if (traditionArr.value?.length) {
      traditionArr.value[0].rowSpan = traditionArr.value.length
    }
    intellectArr.value = content.filter(v => v.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V)
    if (intellectArr.value?.length) {
      intellectArr.value[0].rowSpan = intellectArr.value.length
    }
    _list = _list.concat(traditionArr.value, intellectArr.value)
  } catch (error) {
    console.log('获取构件特征定义失败', error)
  } finally {
    list.value = _list
    lineTypeChange(productionLineTypeEnum.value)
    tableLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #eef7ea;
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
  min-height: 40px;
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
.div-ellipsis{
  width:100%;
  white-space:nowrap;
  overflow:hidden;
  text-overflow:ellipsis;
}
</style>
