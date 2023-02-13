<template>
  <div>
    <common-drawer ref="drawerRef" :title="`检测费用详情`" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="70%">
      <template #titleAfter>
        <!-- <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
        重置
      </common-button> -->
      </template>
      <template #content>
        <!--表格渲染-->
        <common-table ref="tableRef" :max-height="maxHeight" :data="detailList" return-source-data style="width: 100%">
          <el-table-column :show-overflow-tooltip="true" prop="index" label="序号" align="center" width="60" type="index" />
          <el-table-column align="center" key="month" prop="month" :show-overflow-tooltip="true" label="月份" width="80">
            <template #default="{ row }">
              <span>{{ row.month }}月</span>
            </template>
          </el-table-column>
          <el-table-column align="center" key="payDate" prop="payDate" :show-overflow-tooltip="true" label="支付日期" width="120">
            <template v-slot="scope">
              <span>{{ scope.row.payDate ? parseTime(scope.row.payDate, '{y}-{m}-{d}') : '-' }}</span>
            </template>
          </el-table-column>
          <el-table-column align="center" key="project" prop="project" :show-overflow-tooltip="true" label="项目" min-width="150px">
            <template v-slot="scope">
              <span>{{ projectNameFormatter(scope.row.project) }}</span>
            </template>
          </el-table-column>
          <el-table-column
            align="center"
            key="testingFeeTypeName"
            prop="testingFeeTypeName"
            :show-overflow-tooltip="true"
            label="检测费类别"
          />
          <el-table-column align="center" key="feeAmount" prop="feeAmount" :show-overflow-tooltip="true" label="费用">
            <template v-slot="scope">
              <span>{{ toThousand(scope.row.feeAmount) }}</span>
            </template>
          </el-table-column>
          <el-table-column align="center" key="attachments" prop="attachments" :show-overflow-tooltip="true" label="附件">
            <template #header>
              <el-tooltip class="item" effect="dark" :content="`双击编号可预览文附件`" placement="top">
                <div style="display: inline-block">
                  <span>附件</span>
                  <i class="el-icon-info" />
                </div>
              </el-tooltip>
            </template>
            <template v-slot="scope">
              <!-- <upload-btn ref="uploadRef" v-model:files="scope.row.files" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :accept="'.pdf,.jpg,.jpeg,.png'"/> -->
              <template v-if="scope.row.attachments && scope.row.attachments.length > 0">
                <div v-for="item in scope.row.attachments" :key="item.id">
                  <div style="cursor: pointer; color: #409eff" @dblclick="attachmentView(item)">{{ item.name }}</div>
                </div>
              </template>
            </template>
          </el-table-column>
        </common-table>
        <!-- 分页 -->
        <el-pagination
          :total="total"
          :current-page="queryPage.pageNumber"
          :page-size="queryPage.pageSize"
          style="margin-top: 8px"
          layout="total, prev, pager, next, sizes"
          @size-change="handleSizeChange"
          @current-change="handleCurrentChange"
        />
      </template>
    </common-drawer>
    <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow = false" />
  </div>
</template>

<script setup>
import { getDetail } from '@/api/contract/expense-entry/testing-cost'
import { defineProps, defineEmits, ref } from 'vue'

import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import { projectNameFormatter } from '@/utils/project'

import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

// import projectCascader from '@comp-base/project-cascader.vue'

const emit = defineEmits(['update:visible'])
const detailList = ref([])
const pdfShow = ref(false)
const currentId = ref()

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
  },
  query: {
    type: Object,
    default: () => {}
  }
})

// 高度
const { maxHeight } = useMaxHeight({
  extraBox: ['.el-drawer__header'],
  wrapperBox: ['.el-drawer__body'],
  navbar: false,
  clientHRepMainH: true,
  paginate: true
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchDetail })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchDetail })

async function fetchDetail() {
  let _list = []
  try {
    const { content = [], totalElements } = await getDetail({
      year: props.query?.year,
      testingFeeTypeId: props.query?.testingFeeTypeId,
      projectId: props.detailData.project?.id,
      ...queryPage
    })
    setTotalPage(totalElements)
    _list = content
  } catch (e) {
    console.log('获取检测费用', e)
  } finally {
    detailList.value = _list
  }
}

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

// 搜索
// function searchQuery() {
//   fetchDetail()
// }
// 重置
// function resetQuery() {
//   projectId.value = undefined
//   fetchDetail()
// }
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>

