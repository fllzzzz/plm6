<!-- TODO:表格打印区分分页与不分页，重新调接口，考虑排序，打印后给出提示 -->
<!-- eslint-disable no-irregular-whitespace -->
<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :show-close="false"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    custom-class="print-template-detail"
    fullscreen
  >
    <template #title>
      <div class="print-template-title">
        <span>{{ crud.status.title }}</span>
        <div>
          <radio-button
            v-model:value="configItem"
            class="config-item-box"
            :options="configItemEnum.ENUM"
            type="enum"
            color="#fff"
            active-color="#606266"
            button-color="#e8f4ff"
          />
          <el-select v-model="contentZoom" placeholder="显示比例" size="small" class="zoom-select">
            <el-option v-for="item in zoomArr" :key="item" :label="`${item}%`" :value="item" />
          </el-select>
          <el-select v-model="contentDataLength" placeholder="数据填充" size="small" class="zoom-select">
            <el-option v-for="item in contentDataArr" :key="item" :label="`${item}条`" :value="item" />
          </el-select>
          <common-button size="small" type="danger" :disabled="!form.type" @click="print">打印预览</common-button>
          <common-button size="small" type="success" :disabled="!form.type" @click="exportXSLX">导出 Excel</common-button>
          <common-button
size="small"
type="warning"
:disabled="!form.type"
:loading="crud.status.cu === 2"
@click="crud.submitCU"
            >保存</common-button
          >
          <common-button size="small" type="" @click="crud.cancelCU">退出</common-button>
        </div>
      </div>
    </template>
    <div class="dialog-middle">
      <div v-loading="!templateOnload" class="middle-left">
        <div class="content" :style="contentStyle" @click.self="configItem = configItemEnum.BASE.V">
          <template v-if="form.type">
            <!-- 表格渲染 -->
            <template v-if="isNotBlank(config)">
              <!-- logo图片 -->
              <el-image
                v-if="logoCfg?.show && logoCfg.url"
                class="logo-info"
                :style="logoStyle"
                :src="logoCfg.url"
                fit="scale-down"
                draggable="true"
                @dragend="imgDrag"
                @click.stop="configItem = configItemEnum.BASE.V"
              />
              <!-- 二维码 -->
              <div
                v-if="qrCfg?.show"
                class="qr-info"
                :style="qrStyle"
                draggable="true"
                @dragend="qrDrag"
                @click.stop="configItem = configItemEnum.BASE.V"
              >
                <qrcode-vue :value="qrContent" :margin="1" />
              </div>
              <!-- 标题 -->
              <div
                v-if="titleCfg?.show"
                class="title-info"
                :style="titleStyle"
                @click.stop="configItem = configItemEnum.TITLE.V"
                v-html="titleHtml"
              />

              <!-- 头部信息 -->
              <div
                v-if="headerCfg?.show"
                class="header-info"
                :style="headerStyle"
                @click.stop="configItem = configItemEnum.HEADER.V"
                v-html="headerHtml"
              />

              <!-- 表格信息 -->
              <div
                v-if="tableCfg && isNotBlank(filterExampleTableData)"
                ref="tableInfo"
                class="table-info"
                :style="tableStyle"
                @click.stop="configItem = configItemEnum.TABLE.V"
              >
                <div v-html="tableCfg.style" />
                <div v-html="tableHtml" />
              </div>

              <!-- 底部信息 -->
              <div
                v-if="footerCfg?.show"
                class="footer-info"
                :style="footerStyle"
                @click.stop="configItem = configItemEnum.FOOTER.V"
                v-html="footerHtml"
              />

              <!-- 页码 -->
              <div
                v-if="pageCfg?.show"
                class="page-info"
                :style="pageStyle"
                @click.stop="configItem = configItemEnum.BASE.V"
                v-html="pageHtml"
              />
            </template>
            <template v-else>
              <span class="table-type-tip">* 请选择表格模板</span>
            </template>
          </template>
          <template v-else>
            <span class="table-type-tip">* 请选择表格</span>
          </template>
        </div>
      </div>
      <div v-loading="!templateOnload" class="middle-right">
        <div>
          <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="115px">
            <!-- 表格基础信息 -->
            <div v-show="configItem == configItemEnum.BASE.V">
              <div class="form-card">
                <el-form-item label="表格类型" prop="type">
                  <tableTypeCascader
                    v-model="form.type"
                    :disabled="crud.status.edit === prepared"
                    filterable
                    show-all-levels
                    style="width: 300px"
                    @change="handleTableTypeChange"
                  />
                </el-form-item>
                <el-form-item v-if="crud.status.add === prepared" label="创建来源">
                  <tableTemplateSelect
                    :table-type="form.type"
                    default
                    filterable
                    style="width: 300px"
                    @change="handleTemplateSelect"
                    @onload="templateOnload = true"
                  />
                </el-form-item>
                <el-form-item label="表格名称" prop="name">
                  <el-input v-model.trim="form.name" type="text" placeholder="请填写表格名称" style="width: 300px" />
                </el-form-item>
                <el-form-item label="备注" prop="remark">
                  <el-input
                    v-model="form.remark"
                    type="textarea"
                    style="width: 300px"
                    :autosize="{ minRows: 2, maxRows: 4 }"
                    placeholder="请输入备注"
                  />
                </el-form-item>
                <el-form-item label="默认模板" prop="isDefault">
                  <el-checkbox v-model="form.isDefault" />
                </el-form-item>
              </div>

              <!-- 表格打印方向配置 -->
              <div class="form-card form-card-inline-block">
                <el-form-item label="打印方向">
                  <common-radio v-model="config.orient" :options="orientEnum.ENUM" type="enum" style="width: 350px" />
                </el-form-item>
              </div>

              <!-- 表格单位配置，基本上使用默认配置 MM, 0 -->
              <!-- <div class="form-card form-card-inline-block">
                <el-form-item label="基本长度单位">
                  <common-select
                    v-model="config.unit"
                    :options="cssUnitEnum.ENUM"
                    type="enum"
                    placeholder="请选择长度单位"
                    style="width: 150px;"
                  />
                </el-form-item>
                <el-form-item label="小数精度">
                  <common-select
                    v-model="config.unitPrecision"
                    :options="cssUnitPrecisionEnum.ENUM"
                    type="enum"
                    placeholder="请选择小数精度"
                    style="width: 150px;"
                  />
                </el-form-item>
              </div> -->

              <transition name="el-fade-in-linear">
                <div v-if="config.unit && form.type" class="form-cards">
                  <!-- 表格宽高 -->
                  <div class="form-card form-card-inline-block">
                    <el-form-item :label="`高度(${config.unit})`">
                      <common-input-number
                        v-model="config.height"
                        :precision="config.unitPrecision"
                        :min="100"
                        controls-position="right"
                        type="text"
                        placeholder="表格高度"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`宽度(${config.unit})`">
                      <common-input-number
                        v-model="config.width"
                        :precision="config.unitPrecision"
                        :min="0"
                        controls-position="right"
                        type="text"
                        placeholder="表格宽度"
                        style="width: 150px"
                      />
                    </el-form-item>
                  </div>
                  <!-- 表格内边距 -->
                  <div class="form-card form-card-inline-block">
                    <el-form-item :label="`上下边距(${config.unit})`">
                      <common-input-number
                        v-model="config.paddingTB"
                        :precision="config.unitPrecision"
                        :min="0"
                        controls-position="right"
                        type="text"
                        placeholder="表格上下边距"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`左右边距(${config.unit})`">
                      <common-input-number
                        v-model="config.paddingLR"
                        :precision="config.unitPrecision"
                        :min="0"
                        controls-position="right"
                        type="text"
                        placeholder="表格左右边距"
                        style="width: 150px"
                      />
                    </el-form-item>
                  </div>
                  <!-- 页码配置 -->
                  <div v-if="pageCfg" class="form-card form-card-inline-block">
                    <vertical-label name="页码" />
                    <text-setting :data="pageCfg" :show-verticle-align="false" class="text-setting" />
                    <br />
                    <el-form-item :label="`字体大小(${config.fontUnit})`">
                      <common-input-number
                        v-model="pageCfg.size"
                        controls-position="right"
                        :min="8"
                        :max="30"
                        type="text"
                        placeholder="字体大小"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`距离底部(${config.unit})`">
                      <common-input-number
                        v-model="pageCfg.bottom"
                        controls-position="right"
                        :min="0"
                        :max="config.height"
                        type="text"
                        placeholder="距离底部"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`格式`">
                      <common-select
                        v-model="pageCfg.format"
                        :options="pageFormatEnum.ENUM"
                        type="enum"
                        placeholder="请选择展示格式"
                        style="width: 150px"
                        @change="handlePageHtmlChange"
                      />
                    </el-form-item>
                  </div>
                  <!-- logo 图片配置 -->
                  <!-- <div v-if="logoCfg" v-loading="logoLoading" class="form-card form-card-inline-block">
                    <el-tooltip
                      effect="light"
                      :content="`图标（logo）设置：\n
                      1. 图标可通知管理员在公司配置中配置。\n
                      2. ★：公司默认图标。`"
                      placement="right"
                    >
                      <vertical-label :name="`图标`" />
                    </el-tooltip>
                    <text-setting
                      :data="logoCfg"
                      show-all-page
                      :show-bold="false"
                      :show-align="false"
                      :show-verticle-align="false"
                      class="text-setting"
                    />
                    <br>
                    <el-form-item :label="`长度(${config.unit})`">
                      <common-input-number
                        v-model="logoCfg.height"
                        :precision="config.unitPrecision"
                        controls-position="right"
                        type="text"
                        :min="0"
                        :max="config.height"
                        placeholder="长度"
                        style="width: 150px;"
                      />
                    </el-form-item>
                    <el-form-item :label="`宽度(${config.unit})`">
                      <common-input-number
                        v-model="logoCfg.width"
                        :precision="config.unitPrecision"
                        controls-position="right"
                        type="text"
                        :min="0"
                        :max="config.width"
                        placeholder="宽度"
                        style="width: 150px;"
                      />
                    </el-form-item>
                    <br>
                    <el-form-item :label="`距离顶部(${config.unit})`">
                      <common-input-number
                        v-model="logoCfg.top"
                        :precision="config.unitPrecision"
                        controls-position="right"
                        type="text"
                        :min="0"
                        :max="config.height"
                        placeholder="距离顶部"
                        style="width: 150px;"
                      />
                    </el-form-item>
                    <el-form-item :label="`距离左侧(${config.unit})`">
                      <common-input-number
                        v-model="logoCfg.left"
                        :precision="config.unitPrecision"
                        controls-position="right"
                        type="text"
                        :min="0"
                        :max="config.width"
                        placeholder="距离左侧"
                        style="width: 150px;"
                      />
                    </el-form-item>
                    <div class="logo-box">
                      <template v-if="isNotBlank(logos)">
                        <div v-for="img in logos" :key="img.id" class="logo-drawer" @click="chooseLogo(img)">
                          <el-image
                            lazy
                            class="logo-img"
                            :src="img.path"
                            fit="scale-down"
                          />
                          <span v-show="logoCfg.url == img.path" class="logo-select"><i class="el-icon-check" /></span>
                          <span v-show="img.isDefault" class="logo-default"><i class="el-icon-star-on" /></span>
                        </div>
                      </template>
                      <el-tag v-else type="danger">logo未上传</el-tag>
                    </div>
                  </div> -->
                  <!-- 二维码配置，部分表格存在该配置 -->
                  <div v-if="qrCfg" v-loading="logoLoading" class="form-card form-card-inline-block">
                    <el-tooltip
                      effect="light"
                      :content="`二维码设置：\n
                      1. 图标可通知管理员在公司配置中配置。\n`"
                      placement="right"
                    >
                      <vertical-label :name="`二维码`" />
                    </el-tooltip>
                    <text-setting
                      :data="qrCfg"
                      show-all-page
                      :show-bold="false"
                      :show-align="false"
                      :show-verticle-align="false"
                      class="text-setting"
                    />
                    <br />
                    <el-form-item :label="`距离顶部(${config.unit})`">
                      <common-input-number
                        v-model="qrCfg.top"
                        :precision="config.unitPrecision"
                        controls-position="right"
                        type="text"
                        :min="0"
                        :max="config.height"
                        placeholder="距离顶部"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`距离左侧(${config.unit})`">
                      <common-input-number
                        v-model="qrCfg.left"
                        :precision="config.unitPrecision"
                        controls-position="right"
                        type="text"
                        :min="0"
                        :max="config.width"
                        placeholder="距离左侧"
                        style="width: 150px"
                      />
                    </el-form-item>
                  </div>
                </div>
              </transition>
            </div>
            <div :key="componentKey">
              <!-- 标题配置 -->
              <div v-show="configItem == configItemEnum.TITLE.V">
                <div v-if="isNotBlank(titleCfg)" class="form-card form-card-inline-block">
                  <vertical-label name="标题" />
                  <text-setting :data="titleCfg" show-all-page class="text-setting" />
                  <br />
                  <el-form-item label="表格名称">
                    <el-input v-model.trim="titleCfg.title" type="text" placeholder="请填写表格名称" style="width: 150px" />
                  </el-form-item>
                  <el-form-item :label="`字体大小(${config.fontUnit})`">
                    <common-input-number
                      v-model="titleCfg.size"
                      controls-position="right"
                      :min="8"
                      :max="40"
                      type="text"
                      placeholder="字体大小"
                      style="width: 150px"
                    />
                  </el-form-item>
                  <el-form-item :label="`高度(${config.unit})`">
                    <common-input-number
                      v-model="titleCfg.height"
                      :precision="config.unitPrecision"
                      controls-position="right"
                      type="text"
                      :min="0"
                      :max="maxHeight"
                      placeholder="高度"
                      style="width: 150px"
                    />
                  </el-form-item>
                </div>
              </div>
              <!-- 表格配置 -->
              <div v-show="configItem == configItemEnum.TABLE.V">
                <template v-if="isNotBlank(tableCfg)">
                  <!-- 列名格式配置 -->
                  <div class="form-card form-card-inline-block">
                    <vertical-label name="列名" />
                    <text-setting
                      :data="tableCfg.th"
                      :show-display="false"
                      :show-align="false"
                      :show-verticle-align="false"
                      class="text-setting"
                    />
                    <el-form-item :label="`字体大小(${config.fontUnit})`">
                      <common-input-number
                        v-model="tableCfg.th.size"
                        controls-position="right"
                        :min="8"
                        :max="20"
                        type="text"
                        placeholder="字体大小"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`行高(${config.fontUnit})`">
                      <common-input-number
                        v-model="tableCfg.th.lineHeight"
                        controls-position="right"
                        :precision="1"
                        :min="1"
                        :max="20"
                        type="text"
                        placeholder="行高"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`上下边距(${config.unit})`">
                      <common-input-number
                        v-model="tableCfg.th.paddingTB"
                        :precision="config.unitPrecision"
                        :min="0"
                        controls-position="right"
                        type="text"
                        placeholder="表头上下边距"
                        style="width: 150px"
                      />
                    </el-form-item>
                  </div>
                  <!-- 表元格式配置 -->
                  <div class="form-card form-card-inline-block">
                    <el-tooltip
                      effect="light"
                      :content="`表元（表格的具体数据）设置：\n
                1. 空值显示：当数据为空时展示的数据，不填则不显示`"
                      placement="right"
                    >
                      <vertical-label name="表元" />
                    </el-tooltip>
                    <text-setting
                      :data="tableCfg.td"
                      :show-display="false"
                      :show-align="false"
                      :show-verticle-align="false"
                      class="text-setting"
                    />
                    <el-form-item :label="`字体大小(${config.fontUnit})`">
                      <common-input-number
                        v-model="tableCfg.td.size"
                        controls-position="right"
                        :min="8"
                        :max="20"
                        type="text"
                        placeholder="字体大小"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`行高(${config.fontUnit})`">
                      <common-input-number
                        v-model="tableCfg.td.lineHeight"
                        controls-position="right"
                        :precision="1"
                        :min="1"
                        :max="20"
                        type="text"
                        placeholder="行高"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`上下边距(${config.unit})`">
                      <common-input-number
                        v-model="tableCfg.td.paddingTB"
                        :precision="config.unitPrecision"
                        :min="0"
                        controls-position="right"
                        type="text"
                        placeholder="表元上下边距"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`空值显示`">
                      <el-input v-model="tableCfg.emptyVal" type="text" placeholder="空值显示" style="width: 130px" />
                    </el-form-item>
                    <el-form-item label="合计" label-width="65px">
                      <el-checkbox v-model="tableCfg.summary.show" />
                    </el-form-item>
                  </div>
                  <!-- 表格序号及其格式配置 -->
                  <div class="form-card form-card-inline-block">
                    <el-tooltip
                      effect="light"
                      :content="`序号设置：\n
                1. 宽度：①当最小宽度和宽度都未设置时，列宽按照内容自动分配；\n
                　　　　 ②当最小宽度和宽度都设置时，以宽度为准\n
                2. 最小宽度：列的最小宽度，若表格有多余的宽度，会按比例分配到最小宽度上\n`"
                      placement="right"
                    >
                      <vertical-label name="序号" />
                    </el-tooltip>
                    <text-setting :data="tableCfg.index" :show-bold="false" :show-verticle-align="false" class="text-setting" />
                    <el-form-item :label="`宽度(${config.unit})`">
                      <common-input-number
                        v-model="tableCfg.index.width"
                        :precision="config.unitPrecision"
                        :min="0"
                        controls-position="right"
                        type="text"
                      />
                    </el-form-item>
                    <el-form-item :label="`最小宽度(${config.unit})`">
                      <common-input-number
                        v-model="tableCfg.index.minWidth"
                        :precision="config.unitPrecision"
                        :min="0"
                        controls-position="right"
                        type="text"
                      />
                    </el-form-item>
                  </div>
                  <!-- 列配置 -->
                  <div class="form-card form-card-inline-block clearfix">
                    <div class="filter-container" style="display: flex; width: 100%; margin-bottom: 15px">
                      <div class="filter-left-box">
                        <common-button type="primary" size="small" @click="addTableField">添加自定义列</common-button>
                      </div>
                      <div class="filter-right-box">
                        <common-button type="warning" size="small" @click="tableAverageWidth()">平均分配宽度</common-button>
                        <common-button type="warning" size="small" @click="tableAverageWidth('minWidth')">平均分配最小宽度</common-button>
                        <common-button type="danger" size="small" @click="clearTableWidth">清除所有宽度</common-button>
                      </div>
                    </div>
                    <el-tooltip
                      effect="light"
                      :content="`表格设置：\n
                1. 表格内容除显示为“/”的单元格皆可编辑（包括标题及日期格式）。\n
                2. 宽度：①当最小宽度和宽度都未设置时，列宽按照内容自动分配；\n
                　　　　 ②当最小宽度和宽度都设置时，以宽度为准；\n
                　　　　 ③设置宽度时，若希望内容在一行显示，可设置一点冗余宽度，避免因不同设备的差别导致打印换行。\n
                3. 最小宽度：列的最小宽度，若表格有多余的宽度，会按比例分配到最小宽度上。\n
                4. 日期格式：YYYY（2020, 4 位数字的年份）\n
                　　　　　　 YY（20, 2 位数字的年份） \n
                　　　　　　 Q（1~4, 年份的季度。将月份设置为季度的第一个月）\n
                　　　　　　 M MM（1~12, 月份的数字, MM<补0>：01）\n
                　　　　　　 MMM MMMM（1月~12月, 月份名称, MMM: 1月, MMMM: 一月）\n
                　　　　　　 D DD（1~31, 月的某天, DD<补0>：01）\n
                　　　　　　 H HH（0~23, 24 小时制）\n
                　　　　　　 h hh（1~12, 小时, 使用 a A 的 12 小时制）\n
                　　　　　　 m mm（0~59, 分钟）\n
                　　　　　　 s ss（0~59, 秒钟）\n
                　　　　　　 a A（上午或下午）\n
                5. 分位符：1000 => 1,000 、 6666.60 => 6,666.6，使用分位符会删除小数末尾多余的0。\n
                6. 单位：重量（g, kg, t）、长度（mm, m）、宽度（mm, mm）、厚度（mm）、金额（元，万元）可修改单位。\n
                7. 小数精度：0~5, 数量类型的值都可修改小数精度。\n
                8. 全称/简称：部分字段会提供“全称简称”功能，但有些全称和简称是相同的。\n
                9. 打印列排序：可通过拖拽表格“行”上下移动来改变排序。\n
                10. 合计项：非数字类型不可合计，无意义字段不可合计（例：厚度）。\n
                11. 虚线处的表格不会打印。\n`"
                      placement="right"
                    >
                      <vertical-label name="列" />
                    </el-tooltip>
                    <common-table ref="tableFieldsTableRef" :data="tableCfg.fields" row-key="key" border style="width: 100%">
                      <el-table-column label="标题" min-width="100">
                        <template v-slot="scope">
                          <el-tooltip :content="scope.row.title" placement="top-start">
                            <el-input
                              v-model="scope.row.title"
                              type="text"
                              placeholder="标题"
                              class="input-border-none input-padding-right-0"
                              style="width: calc(100% - 23px)"
                            />
                          </el-tooltip>
                        </template>
                      </el-table-column>
                      <el-table-column label="显示" min-width="60" align="center">
                        <template v-slot="scope">
                          <common-button
                            v-if="scope.row.source === dataSourceEnum.CUSTOMIZE.V"
                            type="danger"
                            icon="el-icon-delete"
                            size="mini"
                            style="padding: 7px 10px"
                            @click.stop="removeTableField(scope.$index)"
                          />
                          <el-checkbox v-else v-model="scope.row.show" />
                        </template>
                      </el-table-column>
                      <el-table-column label="对齐方式" min-width="105">
                        <template v-slot="scope">
                          <common-select
                            v-model="scope.row.align"
                            :options="alignEnum.ENUM"
                            type="enum"
                            placeholder="对齐方式"
                            class="input-border-none"
                          />
                        </template>
                      </el-table-column>
                      <el-table-column :label="`宽度(${config.unit})`" min-width="100" align="center">
                        <template v-slot="scope">
                          <common-input-number
                            v-if="!scope.row.children"
                            v-model="scope.row.width"
                            :precision="config.unitPrecision"
                            :min="0"
                            controls-position="right"
                            type="text"
                            class="input-border-none input-number-center"
                          />
                        </template>
                      </el-table-column>
                      <el-table-column :label="`最小宽度(${config.unit})`" min-width="120" align="center">
                        <template v-slot="scope">
                          <common-input-number
                            v-if="!scope.row.children"
                            v-model="scope.row.minWidth"
                            :precision="config.unitPrecision"
                            :min="0"
                            controls-position="right"
                            type="text"
                            class="input-border-none input-number-center"
                          />
                        </template>
                      </el-table-column>
                      <el-table-column label="格式" min-width="240" align="center">
                        <template v-slot="scope">
                          <template v-if="!scope.row.children">
                            <span v-if="!needFormatTypes.includes(scope.row.type)">/</span>
                            <template v-else-if="isNotBlank(scope.row.format)">
                              <template v-if="scope.row.type == fieldTypeEnum.PROJECT.K">
                                <el-checkbox v-model="scope.row.format.showProjectFullName" style="margin-right: 10px">
                                  项目全称
                                </el-checkbox>
                                <el-checkbox v-model="scope.row.format.showContractNo"> 合同编号 </el-checkbox>
                              </template>
                              <el-input
                                v-if="dateTypes.includes(scope.row.type)"
                                v-model="scope.row.format"
                                type="text"
                                placeholder="填写日期格式"
                                class="input-border-none input-center"
                                style="width: 270px"
                              />
                              <span v-if="numberTypes.includes(scope.row.type)" class="delimiter">
                                <span v-if="toThousandfoldTypes.includes(scope.row.type)">
                                  <el-checkbox v-model="scope.row.format.toThousandFilter"> 分位符 </el-checkbox>
                                </span>
                                <span v-if="unitSelectableTypes.includes(scope.row.type)">
                                  <common-select
                                    v-model="scope.row.format.unit"
                                    :options="unitEnumMap[scope.row.type]"
                                    type="enum"
                                    placeholder="单位"
                                    style="width: 60px"
                                    class="input-border-none input-center"
                                  />
                                </span>
                                <span>
                                  <el-tooltip content="小数精度" placement="top-start">
                                    <common-input-number
                                      v-model="scope.row.format.precision"
                                      :precision="0"
                                      :min="0"
                                      :max="5"
                                      controls-position="right"
                                      type="text"
                                      style="width: 60px"
                                      class="input-border-none input-number-center"
                                    />
                                  </el-tooltip>
                                </span>
                                <span v-if="scope.row.type == fieldTypeEnum.METE.K">
                                  <el-checkbox v-model="scope.row.format.showUnit">显示单位</el-checkbox>
                                </span>
                              </span>
                              <template v-if="scope.row.type == fieldTypeEnum.ENUM.K">
                                <el-select v-model="scope.row.format.key" placeholder="名称" class="input-border-none input-center">
                                  <el-option label="全称" value="L" />
                                  <el-option label="简称" value="SL" />
                                </el-select>
                              </template>
                            </template>
                          </template>
                        </template>
                      </el-table-column>
                      <el-table-column label="合计项" min-width="70" align="center">
                        <template v-slot="scope">
                          <el-checkbox
                            v-if="!scope.row.children"
                            v-model="scope.row.sum"
                            :disabled="!sumableTypes.includes(scope.row.type)"
                          />
                        </template>
                      </el-table-column>
                    </common-table>
                  </div>
                </template>
              </div>
              <div v-show="configItem == configItemEnum.HEADER.V">
                <template v-if="isNotBlank(headerCfg)">
                  <div class="form-card form-card-inline-block">
                    <vertical-label name="表头" />
                    <text-setting :data="headerCfg" show-all-page class="text-setting" />
                    <br />
                    <el-form-item :label="`字体大小(${config.fontUnit})`">
                      <common-input-number
                        v-model="headerCfg.size"
                        controls-position="right"
                        :min="8"
                        :max="20"
                        type="text"
                        placeholder="字体大小"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`高度(${config.unit})`">
                      <common-input-number
                        v-model="headerCfg.height"
                        :precision="config.unitPrecision"
                        controls-position="right"
                        type="text"
                        :min="0"
                        :max="maxHeight"
                        placeholder="高度"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`宽度(${config.unit})`">
                      <common-input-number
                        v-model:value="headerCfg.width"
                        :precision="config.unitPrecision"
                        controls-position="right"
                        type="text"
                        :min="0"
                        :max="maxWidth"
                        placeholder="高度"
                        style="width: 150px"
                      />
                    </el-form-item>
                  </div>
                  <div class="form-card form-card-inline-block clearfix">
                    <common-button
type="primary"
class="fr"
size="small"
style="margin-bottom: 15px"
@click="addHeaderField"
                      >添加自定义字段</common-button
                    >
                    <el-tooltip
                      effect="light"
                      :content="`表格设置：\n
                1. 表格内容除显示为“/”的单元格皆可编辑（包括标题及日期格式）。\n
                2. 内容超出宽度，超出部分显示为“...”\n
                3. 宽度：① 当最大宽度和宽度都未设置时，宽度为内容长度；\n
                　　　　 ② 当宽度和最大宽度都设置时，取其中小的一方。\n
                4. 最大宽度：若当前内容比最大宽度小，则宽度相应减少\n
                5. 日期格式：YYYY（2020, 4 位数字的年份）\n
                　　　　　　 YY（20, 2 位数字的年份） \n
                　　　　　　 Q（1~4, 年份的季度。将月份设置为季度的第一个月）\n
                　　　　　　 M MM（1~12, 月份的数字, MM<补0>：01）\n
                　　　　　　 MMM MMMM（1月~12月, 月份名称, MMM: 1月, MMMM: 一月）\n
                　　　　　　 D DD（1~31, 月的某天, DD<补0>：01）\n
                　　　　　　 H HH（0~23, 24 小时制）\n
                　　　　　　 h hh（1~12, 小时, 使用 a A 的 12 小时制）\n
                　　　　　　 m mm（0~59, 分钟）\n
                　　　　　　 s ss（0~59, 秒钟）\n
                　　　　　　 a A（上午或下午）\n
                6. 分位符：1000 => 1,000 、 6666.60 => 6,666.6，使用分位符会删除小数末尾多余的0。\n
                7. 单位：重量（g, kg, t）、长度（mm, m）、宽度（mm, mm）、厚度（mm）、金额（元，万元）可修改单位。\n
                8. 小数精度：0~5, 数量类型的值都可修改小数精度。\n
                9. 全称/简称：部分字段会提供“全称简称”功能，但有些全称和简称是相同的。\n
                10. 打印列排序：可通过拖拽表格“行”上下移动来改变排序。\n`"
                      placement="right"
                    >
                      <vertical-label name="字段" />
                    </el-tooltip>
                    <common-table ref="headerFieldsTableRef" :data="headerCfg.fields" row-key="key" border style="width: 100%">
                      <el-table-column label="标题" min-width="100">
                        <template v-slot="scope">
                          <el-tooltip :content="scope.row.title" placement="top-start">
                            <el-input
                              v-model="scope.row.title"
                              type="text"
                              placeholder="填写标题"
                              class="input-border-none input-padding-right-0"
                            />
                          </el-tooltip>
                        </template>
                      </el-table-column>
                      <el-table-column label="显示" min-width="60" align="center">
                        <template v-slot="scope">
                          <common-button
                            v-if="scope.row.source === dataSourceEnum.CUSTOMIZE.V"
                            type="danger"
                            icon="el-icon-delete"
                            size="mini"
                            style="padding: 7px 10px"
                            @click.stop="removeHeaderField(scope.$index)"
                          />
                          <el-checkbox v-else v-model="scope.row.show" />
                        </template>
                      </el-table-column>
                      <el-table-column :label="`宽度(${config.unit})`" min-width="100" align="center">
                        <template v-slot="scope">
                          <common-input-number
                            v-model="scope.row.width"
                            :precision="config.unitPrecision"
                            :min="headerFieldMinWidth"
                            :max="headerCfg.width"
                            controls-position="right"
                            type="text"
                            class="input-border-none input-number-center"
                          />
                        </template>
                      </el-table-column>
                      <el-table-column :label="`最大宽度(${config.unit})`" min-width="120" align="center">
                        <template v-slot="scope">
                          <span v-if="scope.row.source === dataSourceEnum.CUSTOMIZE.V">/</span>
                          <common-input-number
                            v-else
                            v-model="scope.row.maxWidth"
                            :precision="config.unitPrecision"
                            :min="headerFieldMinWidth"
                            :max="headerCfg.width"
                            controls-position="right"
                            type="text"
                            class="input-border-none input-number-center"
                          />
                        </template>
                      </el-table-column>
                      <el-table-column label="格式" min-width="240" align="center">
                        <template v-slot="scope">
                          <span v-if="!needFormatTypes.includes(scope.row.type)">/</span>
                          <template v-else-if="isNotBlank(scope.row.format)">
                            <template v-if="scope.row.type == fieldTypeEnum.PROJECT.K">
                              <el-checkbox v-model="scope.row.format.showProjectFullName" style="margin-right: 10px">
                                项目全称
                              </el-checkbox>
                              <el-checkbox v-model="scope.row.format.showContractNo"> 合同编号 </el-checkbox>
                            </template>
                            <el-input
                              v-if="dateTypes.includes(scope.row.type)"
                              v-model="scope.row.format"
                              type="text"
                              placeholder="填写日期格式"
                              class="input-border-none input-center"
                              style="width: 270px"
                            />
                            <span v-if="numberTypes.includes(scope.row.type)" class="delimiter">
                              <span v-if="toThousandfoldTypes.includes(scope.row.type)">
                                <el-checkbox v-model="scope.row.format.toThousandFilter"> 分位符 </el-checkbox>
                              </span>
                              <span v-if="unitSelectableTypes.includes(scope.row.type)">
                                <common-select
                                  v-model="scope.row.format.unit"
                                  :options="unitEnumMap[scope.row.type]"
                                  type="enum"
                                  placeholder="单位"
                                  style="width: 60px"
                                  class="input-border-none input-center"
                                />
                              </span>
                              <span>
                                <el-tooltip content="小数精度" placement="top-start">
                                  <common-input-number
                                    v-model="scope.row.format.precision"
                                    :precision="0"
                                    :min="0"
                                    :max="5"
                                    controls-position="right"
                                    type="text"
                                    style="width: 60px"
                                    class="input-border-none input-number-center"
                                  />
                                </el-tooltip>
                              </span>
                            </span>
                            <template v-if="scope.row.type == fieldTypeEnum.METE.K">
                              <el-checkbox v-model="scope.row.format.showUnit">显示单位</el-checkbox>
                            </template>
                            <template v-if="scope.row.type == fieldTypeEnum.ENUM.K">
                              <el-select v-model="scope.row.format.key" placeholder="名称" class="input-border-none input-center">
                                <el-option label="全称" value="L" />
                                <el-option label="简称" value="SL" />
                              </el-select>
                            </template>
                          </template>
                        </template>
                      </el-table-column>
                    </common-table>
                  </div>
                </template>
              </div>
              <div v-show="configItem == configItemEnum.FOOTER.V">
                <template v-if="isNotBlank(footerCfg)">
                  <div class="form-card form-card-inline-block">
                    <el-tooltip
                      effect="light"
                      :content="`表尾设置：\n
                    尽量将高度调节到自己内容所需的高度，以避免额外高度从而产生的空白页`"
                    >
                      <vertical-label name="表尾" />
                    </el-tooltip>
                    <text-setting :data="footerCfg" show-all-page class="text-setting" />
                    <br />
                    <el-form-item :label="`字体大小(${config.fontUnit})`">
                      <common-input-number
                        v-model="footerCfg.size"
                        controls-position="right"
                        :min="8"
                        :max="20"
                        type="text"
                        placeholder="字体大小"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`高度(${config.unit})`">
                      <common-input-number
                        v-model="footerCfg.height"
                        :precision="config.unitPrecision"
                        controls-position="right"
                        type="text"
                        :min="0"
                        :max="maxHeight"
                        placeholder="高度"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`宽度(${config.unit})`">
                      <common-input-number
                        v-model="footerCfg.width"
                        :precision="config.unitPrecision"
                        controls-position="right"
                        type="text"
                        :min="0"
                        :max="maxWidth"
                        placeholder="高度"
                        style="width: 150px"
                      />
                    </el-form-item>
                  </div>
                  <div class="form-card form-card-inline-block">
                    <vertical-label name="提示" />
                    <text-setting :data="footerCfg.tip" :show-verticle-align="false" class="text-setting" />
                    <el-form-item :label="`字体大小(${config.fontUnit})`">
                      <common-input-number
                        v-model="footerCfg.tip.size"
                        controls-position="right"
                        :min="8"
                        :max="20"
                        type="text"
                        placeholder="字体大小"
                        style="width: 150px"
                      />
                    </el-form-item>
                    <el-form-item :label="`在字段上方显示`">
                      <el-checkbox v-model="footerCfg.tip.above" />
                    </el-form-item>
                    <br />
                    <div style="width: 100%; padding: 0 20px; margin-top: 10px">
                      <el-input
                        v-model="footerCfg.tip.text"
                        type="textarea"
                        :autosize="{ minRows: 2, maxRows: 4 }"
                        placeholder="请输入内容"
                      />
                    </div>
                  </div>
                  <div class="form-card form-card-inline-block clearfix">
                    <common-button
type="primary"
class="fr"
size="small"
style="margin-bottom: 15px"
@click="addFooterField"
                      >添加自定义字段</common-button
                    >
                    <el-tooltip
                      effect="light"
                      :content="`表格设置：\n
                1. 表格内容除显示为“/”的单元格皆可编辑（包括标题及日期格式）。\n
                2. 内容超出宽度，超出部分显示为“...”\n
                3. 宽度：① 当最大宽度和宽度都未设置时，宽度为内容长度；\n
                　　　　 ② 当宽度和最大宽度都设置时，取其中小的一方。\n
                4. 最大宽度：若当前内容比最大宽度小，则宽度相应减少\n
                5. 日期格式：YYYY（2020, 4 位数字的年份）\n
                　　　　　　 YY（20, 2 位数字的年份） \n
                　　　　　　 Q（1~4, 年份的季度。将月份设置为季度的第一个月）\n
                　　　　　　 M MM（1~12, 月份的数字, MM<补0>：01）\n
                　　　　　　 MMM MMMM（1月~12月, 月份名称, MMM: 1月, MMMM: 一月）\n
                　　　　　　 D DD（1~31, 月的某天, DD<补0>：01）\n
                　　　　　　 H HH（0~23, 24 小时制）\n
                　　　　　　 h hh（1~12, 小时, 使用 a A 的 12 小时制）\n
                　　　　　　 m mm（0~59, 分钟）\n
                　　　　　　 s ss（0~59, 秒钟）\n
                　　　　　　 a A（上午或下午）\n
                6. 分位符：1000 => 1,000 、 6666.60 => 6,666.6，使用分位符会删除小数末尾多余的0。\n
                7. 单位：重量（g, kg, t）、长度（mm, m）、宽度（mm, mm）、厚度（mm）、金额（元，万元）可修改单位。\n
                8. 小数精度：0~5, 数量类型的值都可修改小数精度。\n
                9. 全称/简称：部分字段会提供“全称简称”功能，但有些全称和简称是相同的。\n
                10. 打印列排序：可通过拖拽表格“行”上下移动来改变排序。\n`"
                      placement="right"
                    >
                      <vertical-label name="字段" />
                    </el-tooltip>
                    <common-table ref="footerFieldsTableRef" :data="footerCfg.fields" row-key="key" border style="width: 100%">
                      <el-table-column label="标题" min-width="100">
                        <template v-slot="scope">
                          <el-tooltip :content="scope.row.title" placement="top-start">
                            <el-input
                              v-model="scope.row.title"
                              type="text"
                              placeholder="填写标题"
                              class="input-border-none input-padding-right-0"
                            />
                          </el-tooltip>
                        </template>
                      </el-table-column>
                      <el-table-column label="显示" min-width="60" align="center">
                        <template v-slot="scope">
                          <common-button
                            v-if="scope.row.source === dataSourceEnum.CUSTOMIZE.V"
                            type="danger"
                            icon="el-icon-delete"
                            size="mini"
                            style="padding: 7px 10px"
                            @click.stop="removeFooterField(scope.$index)"
                          />
                          <el-checkbox v-else v-model="scope.row.show" />
                        </template>
                      </el-table-column>
                      <el-table-column :label="`宽度(${config.unit})`" min-width="100" align="center">
                        <template v-slot="scope">
                          <common-input-number
                            v-model="scope.row.width"
                            :precision="config.unitPrecision"
                            :min="headerFieldMinWidth"
                            :max="footerCfg.width"
                            controls-position="right"
                            type="text"
                            class="input-border-none input-number-center"
                          />
                        </template>
                      </el-table-column>
                      <el-table-column :label="`最大宽度(${config.unit})`" min-width="120" align="center">
                        <template v-slot="scope">
                          <span v-if="scope.row.source === dataSourceEnum.CUSTOMIZE.V">/</span>
                          <common-input-number
                            v-else
                            v-model="scope.row.maxWidth"
                            :precision="config.unitPrecision"
                            :min="headerFieldMinWidth"
                            :max="footerCfg.width"
                            controls-position="right"
                            type="text"
                            class="input-border-none input-number-center"
                          />
                        </template>
                      </el-table-column>
                      <el-table-column label="格式" min-width="240" align="center">
                        <template v-slot="scope">
                          <span v-if="!needFormatTypes.includes(scope.row.type)">/</span>
                          <template v-else-if="isNotBlank(scope.row.format)">
                            <template v-if="scope.row.type == fieldTypeEnum.PROJECT.K">
                              <el-checkbox v-model="scope.row.format.showProjectFullName" style="margin-right: 10px">
                                项目全称
                              </el-checkbox>
                              <el-checkbox v-model="scope.row.format.showContractNo"> 合同编号 </el-checkbox>
                            </template>
                            <el-input
                              v-if="dateTypes.includes(scope.row.type)"
                              v-model="scope.row.format"
                              type="text"
                              placeholder="填写日期格式"
                              class="input-border-none input-center"
                              style="width: 270px"
                            />
                            <span v-if="numberTypes.includes(scope.row.type)" class="delimiter">
                              <span v-if="toThousandfoldTypes.includes(scope.row.type)">
                                <el-checkbox v-model="scope.row.format.toThousandFilter"> 分位符 </el-checkbox>
                              </span>
                              <span v-if="unitSelectableTypes.includes(scope.row.type)">
                                <common-select
                                  v-model="scope.row.format.unit"
                                  :options="unitEnumMap[scope.row.type]"
                                  type="enum"
                                  placeholder="单位"
                                  style="width: 60px"
                                  class="input-border-none input-center"
                                />
                              </span>
                              <span>
                                <el-tooltip content="小数精度" placement="top-start">
                                  <common-input-number
                                    v-model="scope.row.format.precision"
                                    :precision="0"
                                    :min="0"
                                    :max="5"
                                    controls-position="right"
                                    type="text"
                                    style="width: 60px"
                                    class="input-border-none input-number-center"
                                  />
                                </el-tooltip>
                              </span>
                            </span>
                            <span v-if="scope.row.type == fieldTypeEnum.METE.K" class="delimiter">
                              <el-checkbox v-model="scope.row.format.showUnit">显示单位</el-checkbox>
                            </span>
                            <template v-if="scope.row.type == fieldTypeEnum.ENUM.K">
                              <el-select v-model="scope.row.format.key" placeholder="名称" class="input-border-none input-center">
                                <el-option label="全称" value="L" />
                                <el-option label="简称" value="SL" />
                              </el-select>
                            </template>
                          </template>
                        </template>
                      </el-table-column>
                    </common-table>
                  </div>
                </template>
              </div>
            </div>
          </el-form>
        </div>
      </div>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref, reactive, computed, watch, nextTick } from 'vue'
import { regForm } from '@compos/use-crud'
import Sortable from 'sortablejs'
import radioButton from '@comp-common/radio-button'
import tableTypeCascader from '@comp-common/print/table-type-cascader'
import tableTemplateSelect from '@comp-common/print/table-template-select'
import textSetting from '@comp-common/print/text-setting'
import verticalLabel from '@comp-common/vertical-label'

import { isBlank, isNotBlank } from '@data-type/index'
import { px2lengthUnit, convertUnits } from '@/utils/convert/unit'
import {
  orientEnum,
  printModeEnum,
  amountUnitEnum,
  lengthUnitEnum,
  weightUnitEnum,
  thicknessUnitEnum,
  alignEnum,
  dataSourceEnum,
  fieldTypeEnum,
  tableConfigItemEnum as configItemEnum,
  pageFormatEnum,
  cssUnitEnum,
  cssUnitPrecisionEnum
} from '@/utils/print/enum'
import { printTable } from '@/utils/print/table'
import downloadXLSX from '@/utils/print/download'
import { convertColumns, delNotDisplayed, getLastColumns, setting } from '@/utils/print/page-handle'
// import { tableTypeEnum, moduleTypeEnum } from '@/utils/print/table/type'
import example from '@/utils/print/default-template/example'
import formatFn from '@/utils/print/format/index'
import moment from 'moment'
import QrcodeVue from 'qrcode.vue'

// 可格式化的字段类型
const needFormatTypes = [
  fieldTypeEnum.RATE.K,
  fieldTypeEnum.PROJECT.K,
  fieldTypeEnum.DATE.K,
  fieldTypeEnum.DATES.K,
  fieldTypeEnum.AMOUNT.K,
  fieldTypeEnum.LENGTH.K,
  fieldTypeEnum.WEIGHT.K,
  fieldTypeEnum.THICKNESS.K,
  fieldTypeEnum.ENUM.K,
  fieldTypeEnum.QUANTITY.K,
  fieldTypeEnum.METE.K
]
const dateTypes = [fieldTypeEnum.DATE.K, fieldTypeEnum.DATES.K]
const numberTypes = [
  fieldTypeEnum.RATE.K,
  fieldTypeEnum.AMOUNT.K,
  fieldTypeEnum.QUANTITY.K,
  fieldTypeEnum.METE.K,
  fieldTypeEnum.LENGTH.K,
  fieldTypeEnum.WEIGHT.K,
  fieldTypeEnum.THICKNESS.K
]
// 可选择单位的字段类型
const unitSelectableTypes = [fieldTypeEnum.AMOUNT.K, fieldTypeEnum.LENGTH.K, fieldTypeEnum.WEIGHT.K, fieldTypeEnum.THICKNESS.K]
// 需要分位符的字段类型
const toThousandfoldTypes = [
  fieldTypeEnum.AMOUNT.K,
  fieldTypeEnum.QUANTITY.K,
  fieldTypeEnum.METE.K,
  fieldTypeEnum.LENGTH.K,
  fieldTypeEnum.WEIGHT.K
]
// 可合计的字段类型
const sumableTypes = [
  fieldTypeEnum.AMOUNT.K,
  fieldTypeEnum.QUANTITY.K,
  fieldTypeEnum.METE.K,
  fieldTypeEnum.LENGTH.K,
  fieldTypeEnum.WEIGHT.K
]
const unitEnumMap = {}
unitEnumMap[fieldTypeEnum.AMOUNT.K] = amountUnitEnum.ENUM
unitEnumMap[fieldTypeEnum.LENGTH.K] = lengthUnitEnum.ENUM
unitEnumMap[fieldTypeEnum.WEIGHT.K] = weightUnitEnum.ENUM
unitEnumMap[fieldTypeEnum.THICKNESS.K] = thicknessUnitEnum.ENUM

const defaultForm = {
  id: undefined,
  name: '',
  key: undefined, // 暂不保存 TODO:修改注意处理
  type: undefined,
  isDefault: false,
  config: {}
}

const baseConfig = {
  unit: cssUnitEnum.MM.V,
  fontUnit: 'pt',
  unitPrecision: cssUnitPrecisionEnum.ZERO.V,
  orient: orientEnum.LONGITUDINAL.V,
  width: 210, // 打印纸的宽度
  height: 297, // 打印纸的高度
  paddingLR: 10, // 左右内边距
  paddingTB: 10 // 上下内边距
}

const formRef = ref()
const tableFieldsTableRef = ref()
const headerFieldsTableRef = ref()
const footerFieldsTableRef = ref()
const zoomArr = ref([50, 60, 70, 75, 80, 90, 100])
const contentZoom = ref(100)
const contentDataArr = ref([0, 10, 20, 30, 50, 100])
const contentDataLength = ref(10)
const configItem = ref(configItemEnum.BASE.V)
const titleHtml = ref('')
const headerHtml = ref('')
const tableHtml = ref('')
const footerHtml = ref('')
const pageHtml = ref('')
const style = ref('')
// const logos = ref([])
const templateOnload = ref(true)
const logoLoading = ref(false)
const qrContent = ref('欢迎来到初鸣智造钢结构生产执行管理系统')
const componentKey = ref(0)
const headerData = ref({})
const footerData = ref({})
const tableData = ref([])

const tableCfg = ref({})
const qrCfg = ref({})
const logoCfg = ref({})
const titleCfg = ref({})
const headerCfg = ref({})
const footerCfg = ref({})
const pageCfg = ref({})

const rules = ref({
  remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }],
  type: [{ required: true, message: '请选择表格', trigger: 'change' }],
  name: [
    { required: true, message: '请填写表格名称', trigger: 'blur' },
    { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' }
  ]
})

let config = reactive({ ...baseConfig })
let tableFieldsCfg = reactive({
  columns: [],
  columnRows: [],
  lastColumns: []
})

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const prepared = ref(CRUD.STATUS.PREPARED)

const pageWidth = computed(() => {
  return orientEnum.TRANSVERSE.V === config.orient ? config.height : config.width
})

const pageHeight = computed(() => {
  return orientEnum.TRANSVERSE.V === config.orient ? config.width : config.height
})

const maxWidth = computed(() => {
  return pageWidth.value && config.paddingLR * 2
})

const maxHeight = computed(() => {
  return pageHeight.value && config.paddingTB * 2
})

const contentStyle = computed(() => {
  const _config = config
  const _contentZoom = contentZoom.value / 100
  if (_config.unit) {
    return {
      transform: `scale(${_contentZoom})`,
      'margin-right': `-${_config.width * (1 - _contentZoom) + _config.unit}`,
      'margin-bottom': `-${_config.height * (1 - _contentZoom) + _config.unit}`,
      width: pageWidth.value + _config.unit,
      height: pageHeight.value + _config.unit,
      padding: `${_config.paddingTB || 0}${_config.unit} ${_config.paddingLR || 0}${_config.unit}`
    }
  } else {
    return {
      transform: `scale(${_contentZoom})`,
      'margin-right': `-${210 * (1 - _contentZoom)}mm`,
      'margin-bottom': `-${297 * (1 - _contentZoom)}mm`,
      width: '210mm',
      height: '297mm',
      padding: `10mm 10mm`
    }
  }
})

const qrStyle = computed(() => {
  const _style = {}
  if (qrCfg.value) {
    const _config = config
    const itemConfig = qrCfg.value
    if (isNotBlank(itemConfig.height)) {
      _style['height'] = `${itemConfig.height}${_config.unit}`
    }
    if (isNotBlank(itemConfig.width)) {
      _style['width'] = `${itemConfig.width}${_config.unit}`
    }
    if (isNotBlank(itemConfig.top)) {
      _style['top'] = `${itemConfig.top}${_config.unit}`
    }
    if (isNotBlank(itemConfig.left)) {
      _style['left'] = `${itemConfig.left}${_config.unit}`
    }
  }
  return _style
})

const logoStyle = computed(() => {
  const _style = {}
  if (logoCfg.value) {
    const _config = config
    const itemConfig = logoCfg.value
    if (isNotBlank(itemConfig.height)) {
      _style['height'] = `${itemConfig.height}${_config.unit}`
    }
    if (isNotBlank(itemConfig.width)) {
      _style['width'] = `${itemConfig.width}${_config.unit}`
    }
    if (isNotBlank(itemConfig.top)) {
      _style['top'] = `${itemConfig.top}${_config.unit}`
    }
    if (isNotBlank(itemConfig.left)) {
      _style['left'] = `${itemConfig.left}${_config.unit}`
    }
  }
  return _style
})

const titleStyle = computed(() => {
  const _style = {}
  if (titleCfg.value) {
    const _config = config
    const itemConfig = titleCfg.value

    if (isNotBlank(itemConfig.size)) {
      _style['font-size'] = `${itemConfig.size}${_config.fontUnit}`
    }
    if (isNotBlank(itemConfig.bold)) {
      _style['font-weight'] = itemConfig.bold
    }
    if (isNotBlank(itemConfig.height)) {
      _style['height'] = `${itemConfig.height}${_config.unit}`
    }
    if (isNotBlank(itemConfig.align)) {
      _style['justify-content'] = setting.flexAlign(itemConfig.align)
    }
    if (isNotBlank(itemConfig.verticleAlign)) {
      _style['align-items'] = setting.verticleAlign(itemConfig.verticleAlign)
    }
  }
  return _style
})

const pageStyle = computed(() => {
  const _style = {}
  if (pageCfg.value) {
    const _config = config
    const itemConfig = pageCfg.value
    if (isNotBlank(_config.paddingLR)) {
      _style['padding-left'] = `${_config.paddingLR || 0}${_config.unit}`
      _style['padding-right'] = `${_config.paddingLR || 0}${config.unit}`
    }
    if (isNotBlank(itemConfig.bottom)) {
      _style['bottom'] = `${itemConfig.bottom}${_config.unit}`
    }
    if (isNotBlank(itemConfig.size)) {
      _style['font-size'] = `${itemConfig.size}${_config.fontUnit}`
    }
    if (isNotBlank(itemConfig.bold)) {
      _style['font-weight'] = itemConfig.bold
    }
    if (isNotBlank(itemConfig.align)) {
      _style['text-align'] = setting.textAlign(itemConfig.align)
    }
  }
  return _style
})

const headerStyle = computed(() => {
  const _style = {}
  const _config = config
  const itemConfig = headerCfg.value
  if (isNotBlank(itemConfig.width)) {
    _style['width'] = `${itemConfig.width}${_config.unit}`
  }
  if (isNotBlank(itemConfig.height)) {
    _style['height'] = `${itemConfig.height}${_config.unit}`
  }
  if (isNotBlank(itemConfig.size)) {
    _style['font-size'] = `${itemConfig.size}${_config.fontUnit}`
  }
  if (isNotBlank(itemConfig.align)) {
    _style['justify-content'] = setting.flexAlign(itemConfig.align)
  }
  if (isNotBlank(itemConfig.verticleAlign)) {
    _style['align-items'] = setting.verticleAlign(itemConfig.verticleAlign)
  }
  return _style
})

const tableStyle = computed(() => {
  const _style = {}
  if (tableCfg.value) {
    const _config = config
    let tHeight = _config.height - _config.paddingTB * 2
    if (_config.title?.show) {
      tHeight -= _config.title.height
    }
    if (_config.header?.show) {
      tHeight -= _config.header.height
    }
    if (_config.footer?.show) {
      tHeight -= _config.footer.height
    }
    _style['max-height'] = `${tHeight}${_config.unit}`
  }
  return _style
})

const filterExampleTableData = computed(() => {
  let _tableData = []
  if (isNotBlank(tableData.value)) {
    const index = contentDataLength.value > tableData.value.length ? tableData.value.length : contentDataLength.value
    _tableData = tableData.value.slice(0, index)
  }
  return _tableData
})

const headerFieldMinWidth = computed(() => {
  return convertUnits(5, cssUnitEnum.MM.V, config.unit)
})

watch(
  () => form.type,
  (value) => {
    if (prepared.value === crud.status.add && isNotBlank(value)) {
      templateOnload.value = false
    }
  }
)

watch(
  () => titleCfg.value.title,
  () => {
    handleTitleHtmlChange()
  },
  { deep: true }
)

watch(
  () => headerCfg.value,
  () => {
    handleHeaderHtmlChange()
  },
  { deep: true }
)

watch(
  [() => footerCfg.value.bold, () => footerCfg.value.fields, () => footerCfg.value.tip],
  () => {
    handleFooterHtmlChange()
  },
  { deep: true }
)

watch(
  [() => tableCfg.value, () => contentDataLength.value],
  () => {
    handleTableHtmlChange()
  },
  { deep: true }
)

CRUD.HOOK.beforeToCU = () => {
  init()
}

CRUD.HOOK.afterToEdit = () => {
  handleTemplateSelect(JSON.parse(form.config))
}

CRUD.HOOK.beforeSubmit = () => {
  form.config = JSON.parse(JSON.stringify(config))
  form.config.type = form.type
  form.config = JSON.stringify(config)
}

function init() {
  configItem.value = configItemEnum.BASE.V
  titleHtml.value = ''
  headerHtml.value = ''
  tableHtml.value = ''
  footerHtml.value = ''
  pageHtml.value = ''
  style.value = ''
  templateOnload.value = true
  logoLoading.value = false
  tableFieldsCfg = reactive({
    columns: [],
    columnRows: [],
    lastColumns: []
  })
  config = Object.assign(config, baseConfig)
}

async function exportXSLX() {
  let param = {
    header: headerData.value,
    table: JSON.parse(JSON.stringify(filterExampleTableData.value)),
    footer: footerData.value,
    qrCode: qrContent.value
  }
  if (form.type && formatFn[form.type]) {
    // 数据装换
    param = await formatFn[form.type](param)
  }
  param.config = JSON.parse(JSON.stringify(config))
  downloadXLSX(param)
}

function clearTableWidth() {
  tableCfg.value.fields &&
    tableCfg.value.fields.forEach((field) => {
      field.width = undefined
      field.minWidth = undefined
    })
}

function tableAverageWidth(key = 'width') {
  let width = maxWidth.value
  const fields = tableCfg.value.fields.filter((field) => field.show)
  let borderNum = fields.length + 1
  if (tableCfg.value.index.show) {
    width -= tableCfg.value.index.width
    borderNum += 1
  }
  width -= px2lengthUnit(borderNum, config.unit, config.unitPrecision) + 2
  if (isNotBlank(fields)) {
    const fWidth = +(width / fields.length).toFloor(config.unitPrecision)
    fields.forEach((field) => {
      field[key] = fWidth
    })
  }
}

function imgDrag(event) {
  config.qrCode.left = px2lengthUnit(event.pageX - 70, config.unit, config.unitPrecision)
  config.qrCode.top = px2lengthUnit(event.pageY - 100, config.unit, config.unitPrecision)
}

function qrDrag(event) {
  config.qrCode.left = px2lengthUnit(event.pageX - 70, config.unit, config.unitPrecision)
  config.qrCode.top = px2lengthUnit(event.pageY - 100, config.unit, config.unitPrecision)
}

// function chooseLogo(img) {
//   config.logo.url = img.path
//   if (isBlank(logoCfg.value.width)) {
//     config.logo.width = 20
//   }
//   if (isBlank(logoCfg.value.height)) {
//     config.logo.height = 20
//   }
//   if (isBlank(logoCfg.value.top)) {
//     config.logo.top = 10
//   }
//   if (isBlank(logoCfg.value.left)) {
//     config.logo.left = 10
//   }
// }

function addTableField() {
  tableCfg.value.fields.push({
    key: moment().format('x'),
    show: true,
    title: '自定义',
    type: fieldTypeEnum.OTHER.K,
    source: dataSourceEnum.CUSTOMIZE.V,
    align: alignEnum.CENTER.V,
    width: 15
  })
}

function removeTableField(index) {
  tableCfg.value.fields.splice(index, 1)
}

function addHeaderField() {
  headerCfg.value.fields.push({
    key: moment().format('x'),
    show: true,
    title: '自定义',
    source: dataSourceEnum.CUSTOMIZE.V,
    width: 60
  })
}

function removeHeaderField(index) {
  headerCfg.value.fields.splice(index, 1)
}

function addFooterField() {
  footerCfg.value.fields.push({
    key: moment().format('x'),
    show: true,
    title: '自定义',
    source: dataSourceEnum.CUSTOMIZE.V,
    width: 60
  })
}

function removeFooterField(index) {
  footerCfg.value.fields.splice(index, 1)
}

// 行拖拽
function rowDrop() {
  const doms = [
    { el: tableFieldsTableRef.value, table: tableCfg.value?.fields },
    { el: headerFieldsTableRef.value, table: headerCfg.value?.fields },
    { el: footerFieldsTableRef.value, table: footerCfg.value?.fields }
  ]
  for (const dom of doms) {
    if (dom.el) {
      const tbody = dom.el.$el.querySelectorAll('.el-table__body-wrapper > table > tbody')[0]
      Sortable.create(tbody, {
        onEnd: ({ newIndex, oldIndex }) => {
          const currRow = dom.table.splice(oldIndex, 1)[0]
          dom.table.splice(newIndex, 0, currRow)
        }
      })
    }
  }
}

function handleTemplateSelect(val) {
  // config = reactive(isNotBlank(val) ? setting.correctJSON(JSON.parse(JSON.stringify(val))) : setting.correctJSON(Object.assign({}, baseConfig)))
  const _config = isNotBlank(val)
    ? setting.correctJSON(JSON.parse(JSON.stringify(val)))
    : setting.correctJSON(Object.assign({}, baseConfig))
  Object.assign(config, { qrCode: {}, ..._config })
  setData()
  splicingHtml()
  componentKey.value++
}

function handleTableTypeChange(val) {
  form.name = val?.L
}

function setData() {
  tableCfg.value = config.table
  qrCfg.value = config.qrCode
  logoCfg.value = config.logo
  titleCfg.value = config.title
  headerCfg.value = config.header
  footerCfg.value = config.footer
  pageCfg.value = config.page

  if (isNotBlank(config.table?.fields)) {
    const columnRows = convertColumns(JSON.parse(JSON.stringify(config.table.fields)))
    const lastColumns = getLastColumns(columnRows)
    tableData.value = example.getList({
      fields: lastColumns,
      extraFields: config.table.extraFields,
      number: 100
    })
  }
  if (isNotBlank(config.header?.fields)) {
    headerData.value = example.getOne({
      fields: config.header.fields,
      extraFields: config.header.extraFields
    })
  }
  if (isNotBlank(config.footer?.fields)) {
    footerData.value = example.getOne({
      fields: config.footer.fields,
      extraFields: config.footer.extraFields
    })
  }
}

async function print() {
  let param = {
    header: headerData.value,
    table: filterExampleTableData.value,
    footer: footerData.value,
    qrCode: qrContent.value
  }
  if (form.type && formatFn[form.type]) {
    // 数据装换
    param = await formatFn[form.type](param)
  }
  param.printMode = printModeEnum.PREVIEW.V
  param.config = JSON.parse(JSON.stringify(config))
  printTable(param)
}

function setColumns() {
  if (isBlank(tableCfg.value)) return
  // 设置column
  tableFieldsCfg.columns = delNotDisplayed(JSON.parse(JSON.stringify(tableCfg.value.fields)))
  tableFieldsCfg.columnRows = convertColumns(tableFieldsCfg.columns)
  tableFieldsCfg.lastColumns = getLastColumns(tableFieldsCfg.columnRows)
}

function splicingHtml() {
  if (isBlank(config)) {
    return
  }
  handleTitleHtmlChange()
  handleHeaderHtmlChange()
  handleTableHtmlChange()
  handleFooterHtmlChange()
  handlePageHtmlChange()
  nextTick(() => {
    rowDrop()
  })
}

function handleTitleHtmlChange() {
  titleHtml.value = setting.getTitleHtml(titleCfg.value) // 拼接页码
}

function handleHeaderHtmlChange() {
  setting.setHeaderFieldStyle(config) // 设置字段信息
  headerHtml.value = setting.getHeaderHtml(headerData.value, headerCfg.value) // 拼接页码
}

function handleTableHtmlChange() {
  setColumns() // 设置字段
  setting.setTableStyle(config, tableFieldsCfg) // 设置表格及其列样式
  tableHtml.value = setting.getTableHtml(filterExampleTableData.value, tableCfg.value, tableFieldsCfg) // 拼接页码
}

function handleFooterHtmlChange() {
  setting.setFooterFieldStyle(config) // 设置字段信息
  footerHtml.value = setting.getFooterHtml(footerData.value, config) // 拼接页码
}

function handlePageHtmlChange() {
  pageHtml.value = setting.getPageHtml(pageCfg.value) // 拼接页码
}
</script>

<style lang="scss">
.print-template-detail {
  background: #f0f0f0;
  .el-dialog__header {
    padding-top: 10px;
    padding-bottom: 10px;
    background-color: #5c93ce;
    color: #ffffff;
  }
  .el-dialog__body {
    padding: 15px 20px 0 20px;
    box-sizing: border-box;
  }
}
</style>
<style lang="scss" scoped>
// 分隔符
.delimiter {
  display: inline-flex;
  align-items: center;
  > span {
    display: inline-flex;
    align-items: center;
    height: 30px;
  }
  > span + span {
    margin-left: 10px;
    position: relative;
    &::before {
      content: '';
      position: absolute;
      background-color: #dcdfe6;
      top: 7px;
      left: -7px;
      width: 1px;
      height: 20px;
    }
  }
}
.logo-box {
  width: 100%;
  margin-left: 20px;
  margin-top: 20px;
  display: flex !important;
  justify-content: flex-start;
  align-items: center;
  flex-wrap: wrap;
  .logo-drawer {
    position: relative;
    display: flex;
    justify-content: center;
    align-items: center;
    width: 80px;
    height: 80px;
    border-radius: 5px;
    background-color: #f5f7fa;
    border: 1px solid #dcdfe6;
    overflow: hidden;
    cursor: pointer;
    user-select: none;
    + .logo-drawer {
      margin-left: 10px;
    }
    > .logo-img {
      width: 78px;
      height: 78px;
    }
  }
  .logo-select {
    background: #11b95c;
    transform: rotate(45deg);
    color: white;
    font-weight: 100;
    position: absolute;
    top: -2px;
    right: -18px;
    width: 50px;
    height: 15px;
    font-size: 11px;
    display: flex;
    justify-content: center;
    align-items: center;
    > .el-icon-check {
      transform: rotate(-45deg);
    }
  }
  .logo-default {
    background: #ff4949;
    transform: rotate(-45deg);
    color: white;
    font-weight: 100;
    position: absolute;
    top: -1px;
    left: -19px;
    width: 50px;
    height: 15px;
    font-size: 11px;
    display: flex;
    justify-content: center;
    align-items: center;
  }
}

::v-deep(.el-table__placeholder) {
  width: 0;
}

::v-deep(.el-button--info) {
  color: #fff;
  background-color: #dcdfe6;
  border-color: #c0c4cc;
}
.input-border-none {
  ::v-deep(.el-input__inner) {
    padding-left: 0;
    background-color: unset;
  }
}
.input-center {
  ::v-deep(.el-input__inner) {
    text-align: center;
    padding-right: 0;
  }
}
.input-padding-right-0 {
  ::v-deep(.el-input__inner) {
    padding-right: 0;
  }
}
.input-number-center {
  ::v-deep(.el-input__inner) {
    text-align: center !important;
    padding-right: 30px;
  }
}
::v-deep(.el-input-number.is-controls-right .el-input__inner) {
  text-align: left;
}

.print-template-title {
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: space-between;
  > span {
    font-size: 18px;
  }
  // .el-button+.el-button {
  //   margin-left: 0;
  // }
  .config-item-box {
    margin-right: 15px;
  }
  .zoom-select {
    width: 100px;
    margin-right: 10px;
    ::v-deep(.el-input__inner) {
      border: none;
      background-color: #e8f4ff;
    }
  }
}
.dialog-middle {
  width: inherit;
  height: calc(100vh - 70px);
  overflow: auto;
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-start;
  // flex-wrap: wrap;
  .middle-right {
    min-width: 600px;
    max-height: calc(100% - 10px);
    overflow: auto;
    width: min-content;
    flex: 1 0 auto;
    margin: 0 10px 10px 10px;
    .form-card {
      position: relative;
      box-sizing: border-box;
      padding: 20px 35px;
      width: 100%;
      background-color: #ffffff;
      &:hover {
        box-shadow: 0 2px 12px 0 rgba(0, 0, 0, 0.1);
      }
      .el-form-item--small.el-form-item {
        padding: 10px 0;
        margin-bottom: 0;
      }
      + .form-card {
        margin-top: 10px;
      }
      + .form-cards {
        margin-top: 10px;
      }
      .text-setting {
        margin: 10px 10px 10px 20px;
      }
    }
    .form-cards {
      + .form-card {
        margin-top: 10px;
      }
      + .form-cards {
        margin-top: 10px;
      }
    }
    .form-card-inline-block {
      > div.el-form-item {
        display: inline-flex;
      }
    }
  }
  .middle-left {
    /* font-feature-settings: 'onum' 1; */
    max-width: 70%;
    margin-bottom: 10px;
    color: #333;
    max-height: calc(100% - 10px);
    overflow: auto;
    flex: 0 1 auto;
    padding: 0 10px;
    text-rendering: optimizeLegibility;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    font-family: AvenirNext, Avenir, -apple-system, BlinkMacSystemFont, Roboto Slab, Droid Serif, Segoe UI, Oxygen-Sans, Ubuntu, Cantarell,
      Georgia, serif;
    .content {
      overflow: hidden;
      transform-origin: 0 0 0; /*以左上角为起点 */
      box-sizing: border-box;
      background-color: #ffffff;
      box-shadow: 0 0 8px 0 rgba(0, 0, 0, 0.2);
      user-select: none;
      .table-type-tip {
        font-weight: bold;
        font-size: 20px;
        color: #ff4949;
      }
      > div {
        cursor: pointer;
        border: 1px solid rgba(255, 255, 255, 0.1);
        box-sizing: border-box;
        &:hover {
          background-color: #f5f5f5;
          border-radius: 5px;
          border: 1px dotted #333;
        }
      }
      .logo-info {
        position: absolute;
        border: none;
      }
      // 标题
      .title-info {
        display: flex;
        justify-content: center;
        align-items: flex-start;
        width: 100%;
        box-sizing: border-box;
        text-align: center;
        font-size: 17pt;
        font-weight: bold;
      }
      // 表头信息
      .header-info {
        font-family: lucida sans unicode, lucida grande, Sans-Serif;
        overflow: hidden;
        width: 100%;
        box-sizing: content-box;
        display: flex;
        flex-direction: row;
        justify-content: flex-start;
        align-items: center;
        flex-wrap: wrap;
        line-height: 1.15;
      }
      ::v-deep(.header-info > div) {
        display: inline-block;
        margin: 1mm 0;
        // padding-right: 5mm;
        box-sizing: border-box;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      // footer
      .footer-info {
        font-family: lucida sans unicode, lucida grande, Sans-Serif;
        overflow: hidden;
        width: 100%;
        box-sizing: content-box;
        display: flex;
        flex-direction: row;
        justify-content: flex-start;
        align-items: center;
        flex-wrap: wrap;
        ::v-deep(.tip) {
          display: inline-block;
          white-space: pre-line;
          width: 100%;
          margin: 1mm 0;
          font-size: 9pt;
          color: red;
        }
      }
      ::v-deep(.footer-info > div) {
        display: inline-block;
        margin: 1mm 0;
        // padding-right: 5mm;
        box-sizing: border-box;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      // 表格
      .table-info {
        box-sizing: border-box;
        overflow: hidden;
        width: 100%;
        margin: 1mm 0;
        ::v-deep(.preview-table) {
          font-family: lucida sans unicode, lucida grande, Sans-Serif;
          font-size: 9pt;
          border-collapse: collapse;
          width: 100%;
          .blank-column {
            min-width: 0;
            border: 1px dashed;
            > div {
              display: inline-block;
              min-width: 0;
            }
          }
        }
        ::v-deep(.preview-table th) {
          padding: 0;
          line-height: 15pt;
          border: 1px solid #000;
          > div {
            padding: 0 1mm;
            box-sizing: border-box;
            min-height: 3mm;
          }
        }
        ::v-deep(.preview-table td) {
          padding: 0;
          border: 1px solid #000;
          line-height: 13pt;
          white-space: pre-wrap;
          > div {
            padding: 0 1mm;
            box-sizing: border-box;
            min-height: 3mm;
          }
        }
        ::v-deep(.preview-table tbody tr:last-child) {
          border-bottom: none;
        }
      }
      .qr-info {
        position: absolute;
        canvas {
          width: 100% !important;
          height: 100% !important;
        }
      }
      .page-info {
        box-sizing: border-box;
        width: 100%;
        position: absolute;
        bottom: 0;
        left: 0;
      }
    }
  }
}
</style>
